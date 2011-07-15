{-# LANGUAGE BangPatterns, DeriveDataTypeable, OverloadedStrings,
    RecordWildCards, ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, getChanContents, newChan, writeChan)
import Control.Exception (IOException, catch)
import Control.Monad (forM_, unless, when)
import Criterion.Analysis (SampleAnalysis(..), analyseSample, scale)
import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Text.Buildable
import Data.Text.Lazy.Builder
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Network.HTTP.Enumerator
import Network.Socket (withSocketsDo)
import Prelude hiding (catch)
import Statistics.Quantile (weightedAvg)
import Statistics.Resampling.Bootstrap (Estimate(..))
import System.Console.CmdArgs
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Format as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified System.Timeout as T

data Args = Args {
      concurrency :: Int
    , num_requests :: Int
    , requests_per_second :: Double
    , timeout :: Double
    , url :: String
    } deriving (Eq, Show, Typeable, Data)

defaultArgs :: Args
defaultArgs = Args {
                concurrency = 1
              , num_requests = 1
              , requests_per_second = def
              , timeout = 60
              , url = def &= argPos 0
              }

data Event =
    HttpResponse {
      respCode :: {-# UNPACK #-} !Int
    , respLength :: {-# UNPACK #-} !Int
    } | NetworkError
      | Timeout
      | Done
    deriving (Eq, Show)

data Summary = Summary {
      summEvent :: Event
    , summElapsed :: {-# UNPACK #-} !Double
    , summStart :: {-# UNPACK #-} !Double
    } deriving (Eq, Show)

summEnd :: Summary -> Double
summEnd Summary{..} = summStart + summElapsed

main :: IO ()
main = withSocketsDo $ do
  as@Args{..} <- cmdArgs defaultArgs
  validateArgs as
  req <- parseUrl url
  let reqs = zipWith (+) (replicate concurrency reqsPerThread)
                         (replicate leftover 1 ++ repeat 0)
        where (reqsPerThread,leftover) = num_requests `quotRem` concurrency
  let !interval | requests_per_second == 0 = 0
                | otherwise = realToFrac (fromIntegral concurrency /
                                          requests_per_second)
  ch <- newChan
  forM_ reqs $ \numReqs -> forkIO . withManager $ \mgr -> do
    let as' = as {
                num_requests = numReqs
              }
    client as' mgr req interval ch
  results <- take num_requests <$> getChanContents ch
  putStrLn "analysing results"
  report =<< analyse results

client :: Args -> Manager -> Request IO -> POSIXTime -> Chan Summary -> IO ()
client Args{..} mgr req interval ch = loop 0 =<< getPOSIXTime
  where
    loop !n now
        | n == num_requests = return ()
        | otherwise = do
      !evt <- timedRequest `catch`
              \(_::IOException) -> closeManager mgr >> return NetworkError
      now' <- getPOSIXTime
      let elapsed = now' - now
      writeChan ch Summary {
                      summEvent = evt
                    , summElapsed = realToFrac elapsed
                    , summStart = realToFrac now'
                    }
      when (elapsed < interval) $
        threadDelay . truncate $ (interval - elapsed) * 1000000
      loop (n+1) =<< getPOSIXTime
    issueRequest = httpLbs req mgr
    timedRequest
      | timeout == 0 = respEvent <$> issueRequest
      | otherwise    = do
      maybeResp <- T.timeout (truncate (timeout * 1e6)) issueRequest
      case maybeResp of
        Just resp -> return (respEvent resp)
        _         -> closeManager mgr >> return Timeout

respEvent :: Response -> Event
respEvent resp = HttpResponse {
                   respCode = statusCode resp
                 , respLength = fromIntegral . L.length . responseBody $ resp
                 }

validateArgs :: Args -> IO ()
validateArgs Args{..} = do
  let p !? what | p         = Nothing
                | otherwise = Just what
      infix 1 !?
      problems = catMaybes [
         concurrency > 0 !? "--concurrency must be positive"
       , num_requests > 0 !? "--num-requests must be positive"
       , requests_per_second >= 0 !? "--requests-per-second cannot be negative"
       , timeout >= 0 !? "--timeout cannot be negative"
       ]
  forM_ problems $ hPutStrLn stderr . ("Error: " ++)
  unless (null problems) $ exitWith (ExitFailure 1)

data Analysis = Analysis {
      latency :: SampleAnalysis
    , latency99 :: Double
    , latency999 :: Double
    , throughput :: SampleAnalysis
    , throughput10 :: Double
    } deriving (Show)

analyse :: [Summary] -> IO Analysis
analyse sums = do
  let sumv = sortBy (compare `on` summStart) . V.fromList $ sums
      start = summStart . G.head $ sumv
      end = summEnd . G.last $ sumv
      elapsed = end - start
      timeSlice = min elapsed 1 / 200
      slices = U.unfoldrN (round (elapsed / timeSlice)) go (sumv,1)
        where go (v,i) = let (a,b) = G.span (\s -> summStart s <= t) v
                             t = start + (i * timeSlice)
                         in Just (fromIntegral $ G.length a,(b,i+1))
      ci = 0.95
      resamples = 10 * 1000
  l <- analyseSample ci (G.convert . G.map summElapsed $ sumv) resamples
  t <- analyseSample ci slices resamples
  return Analysis {
                 latency = l
               , latency99 = weightedAvg 99 100 . G.map summElapsed $ sumv
               , latency999 = weightedAvg 999 1000 . G.map summElapsed $ sumv
               , throughput = scale (recip timeSlice) t
               , throughput10 = (/ timeSlice) . weightedAvg 10 100 $ slices
    }

time :: Double -> Builder
time t
     | t < 1e-3  = build (t * 1e6) `mappend` " usec"
     | t < 1     = build (t * 1e3) `mappend` " msec"
     | otherwise = build t `mappend` " sec"

report :: Analysis -> IO ()
report Analysis{..} = do
  T.print "latency:\n    mean:    {}\n    std dev: {}\n"
    (time (estPoint (anMean latency)), time (estPoint (anStdDev latency)))
  T.print "    99%:     {}\n    99.9%:   {}\n" (time latency99, time latency999)
  T.print "\nthroughput:\n    mean:    {} req/sec\n    std dev: {} req/sec\n"
    (estPoint (anMean throughput), estPoint (anStdDev throughput))
  T.print "    10%:     {} req/sec\n" [throughput10]

-- | Sort a vector.
sortBy :: (G.Vector v e) => I.Comparison e -> v e -> v e
sortBy cmp = G.modify (I.sortBy cmp)
{-# INLINE sortBy #-}
