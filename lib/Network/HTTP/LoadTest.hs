{-# LANGUAGE BangPatterns, DeriveDataTypeable, OverloadedStrings,
    RecordWildCards, ScopedTypeVariables #-}

module Network.HTTP.LoadTest
    (
    -- * Running a load test
      Config(..)
    , defaultConfig
    , run
    -- * Results
    , Event(..)
    , Summary(..)
    , summEnd
    -- * Result analysis
    , Analysis(..)
    , analyse
    , report
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (IOException, catch)
import Control.Monad (forM_, replicateM, when)
import Criterion.Analysis (OutlierEffect(..), OutlierVariance(..),
                           SampleAnalysis(..), analyseSample, scale)
import Data.Data (Data)
import Data.Function (on)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Text.Buildable (build)
import Data.Text.Lazy.Builder (Builder)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Typeable (Typeable)
import Network.HTTP.Enumerator
import Prelude hiding (catch)
import Statistics.Quantile (weightedAvg)
import Statistics.Resampling.Bootstrap (Estimate(..))
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Format as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified System.Timeout as T

data Config = Config {
      concurrency :: Int
    , numRequests :: Int
    , requestsPerSecond :: Double
    , timeout :: Double
    , url :: String
    } deriving (Eq, Read, Show, Typeable, Data)

defaultConfig :: Config
defaultConfig = Config {
                concurrency = 1
              , numRequests = 1
              , requestsPerSecond = 0
              , timeout = 60
              , url = ""
              }

data Event =
    HttpResponse {
      respCode :: {-# UNPACK #-} !Int
    , respLength :: {-# UNPACK #-} !Int
    } | NetworkError
      | Timeout
      | Done
    deriving (Eq, Read, Show, Typeable, Data)

data Summary = Summary {
      summEvent :: Event
    , summElapsed :: {-# UNPACK #-} !Double
    , summStart :: {-# UNPACK #-} !Double
    } deriving (Eq, Read, Show, Typeable, Data)

summEnd :: Summary -> Double
summEnd Summary{..} = summStart + summElapsed

run :: Config -> IO (V.Vector Summary)
run cfg@Config{..} = do
  req <- parseUrl url
  let reqs = zipWith (+) (replicate concurrency reqsPerThread)
                         (replicate leftover 1 ++ repeat 0)
        where (reqsPerThread,leftover) = numRequests `quotRem` concurrency
  let !interval | requestsPerSecond == 0 = 0
                | otherwise = realToFrac (fromIntegral concurrency /
                                          requestsPerSecond)
  ch <- newChan
  forM_ reqs $ \numReqs -> forkIO . withManager $ \mgr -> do
    let cfg' = cfg {
                numRequests = numReqs
              }
    writeChan ch =<< client cfg' mgr req interval
  V.concat <$> replicateM concurrency (readChan ch)

client :: Config -> Manager -> Request IO -> POSIXTime
       -> IO (V.Vector Summary)
client Config{..} mgr req interval = loop 0 [] =<< getPOSIXTime
  where
    loop !n acc now
        | n == numRequests = return $! V.fromList (reverse acc)
        | otherwise = do
      !evt <- timedRequest `catch`
              \(_::IOException) -> closeManager mgr >> return NetworkError
      now' <- getPOSIXTime
      let elapsed = now' - now
          !s = Summary {
                 summEvent = evt
               , summElapsed = realToFrac elapsed
               , summStart = realToFrac now'
               }
      when (elapsed < interval) $
        threadDelay . truncate $ (interval - elapsed) * 1000000
      loop (n+1) (s:acc) =<< getPOSIXTime
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

data Analysis = Analysis {
      latency :: SampleAnalysis
    , latency99 :: Double
    , latency999 :: Double
    , throughput :: SampleAnalysis
    , throughput10 :: Double
    } deriving (Eq, Show, Typeable, Data)

analyse :: V.Vector Summary -> IO Analysis
analyse sums = do
  let sumv = sortBy (compare `on` summStart) sums
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
  effect (anOutliers latency)
  T.print "    99%:     {}\n    99.9%:   {}\n" (time latency99, time latency999)
  T.print "\nthroughput:\n    mean:    {} req/sec\n    std dev: {} req/sec\n"
    (estPoint (anMean throughput), estPoint (anStdDev throughput))
  effect (anOutliers throughput)
  T.print "    10%:     {} req/sec\n" [throughput10]

effect :: OutlierVariance -> IO ()
effect OutlierVariance{..} =
    case ovEffect of
      Unaffected -> return ()
      _ -> T.print "    estimates {} affected by outliers ({}%)\n"
           (howMuch, T.fixed 1 (ovFraction * 100))
    where howMuch = case ovEffect of
                      Unaffected -> "not" :: Text
                      Slight     -> "slightly"
                      Moderate   -> "moderately"
                      Severe     -> "severely"

-- | Sort a vector.
sortBy :: (G.Vector v e) => I.Comparison e -> v e -> v e
sortBy cmp = G.modify (I.sortBy cmp)
{-# INLINE sortBy #-}
