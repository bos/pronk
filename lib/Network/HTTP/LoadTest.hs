{-# LANGUAGE BangPatterns, DeriveDataTypeable, OverloadedStrings,
    RecordWildCards, ScopedTypeVariables #-}

module Network.HTTP.LoadTest
    (
    -- * Running a load test
      Config(..)
    , NetworkError(..)
    , defaultConfig
    , run
    -- * Results
    , Event(..)
    , Summary(..)
    , summEnd
    -- * Result analysis
    , Analysis(..)
    , analyse
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (Exception, IOException, catch, throwIO, try)
import Control.Monad (forM_, replicateM, when)
import Criterion.Analysis (SampleAnalysis(..), analyseSample, scale)
import Data.Data (Data)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (nub)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Typeable (Typeable)
import Network.HTTP.Enumerator
import Prelude hiding (catch)
import Statistics.Quantile (weightedAvg)
import qualified Data.ByteString.Lazy as L
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
    } | Timeout
      | Done
    deriving (Eq, Read, Show, Typeable, Data)

-- | Exception thrown if issuing a HTTP request fails.
data NetworkError = NetworkError {
      fromNetworkError :: IOException
    } deriving (Eq, Show, Typeable)

instance Exception NetworkError

data Summary = Summary {
      summEvent :: Event
    , summElapsed :: {-# UNPACK #-} !Double
    , summStart :: {-# UNPACK #-} !Double
    } deriving (Eq, Read, Show, Typeable, Data)

summEnd :: Summary -> Double
summEnd Summary{..} = summStart + summElapsed

run :: Config -> IO (Either [NetworkError] (V.Vector Summary))
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
    writeChan ch =<< try (client cfg' mgr req interval)
  (errs,vs) <- partitionEithers <$> replicateM concurrency (readChan ch)
  return $ case errs of
             [] -> Right (V.concat vs)
             _  -> Left (nub errs)

client :: Config -> Manager -> Request IO -> POSIXTime
       -> IO (V.Vector Summary)
client Config{..} mgr req interval = loop 0 [] =<< getPOSIXTime
  where
    loop !n acc now
        | n == numRequests = return $! V.fromList (reverse acc)
        | otherwise = do
      !evt <- timedRequest
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
    issueRequest = httpLbs req mgr `catch` (throwIO . NetworkError)
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

-- | Sort a vector.
sortBy :: (G.Vector v e) => I.Comparison e -> v e -> v e
sortBy cmp = G.modify (I.sortBy cmp)
{-# INLINE sortBy #-}
