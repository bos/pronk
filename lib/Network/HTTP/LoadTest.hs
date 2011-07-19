{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Network.HTTP.LoadTest
    (
    -- * Running a load test
      NetworkError(..)
    , Config(..)
    , Req(..)
    , defaultConfig
    , run
    -- * Result analysis
    , Analysis(..)
    , Basic(..)
    , analyseBasic
    , analyseFull
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (catch, throwIO, try)
import Control.Monad (forM_, replicateM, when)
import Criterion.Analysis (SampleAnalysis, analyseSample, scale)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (nub)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Network.HTTP.Enumerator
import Network.HTTP.LoadTest.Types
import Prelude hiding (catch)
import Statistics.Quantile (weightedAvg)
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Sample as S
import qualified System.Timeout as T

run :: Config -> IO (Either [NetworkError] (V.Vector Summary))
run cfg@Config{..} = do
  let reqs = zipWith (+) (replicate concurrency reqsPerThread)
                         (replicate leftover 1 ++ repeat 0)
        where (reqsPerThread,leftover) = numRequests `quotRem` concurrency
  let !interval | requestsPerSecond == 0 = 0
                | otherwise = realToFrac (fromIntegral concurrency /
                                          requestsPerSecond)
  ch <- newChan
  forM_ reqs $ \numReqs -> forkIO . withManager $ \mgr -> do
    let cfg' = cfg { numRequests = numReqs }
    writeChan ch =<< try (client cfg' mgr interval)
  (errs,vs) <- partitionEithers <$> replicateM concurrency (readChan ch)
  return $ case errs of
             [] -> Right (V.concat vs)
             _  -> Left (nub errs)

client :: Config -> Manager -> POSIXTime
       -> IO (V.Vector Summary)
client Config{..} mgr interval = loop 0 [] =<< getPOSIXTime
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
    issueRequest = httpLbs (fromReq request) mgr
                   `catch` (throwIO . NetworkError)
    timedRequest
      | timeout == 0 = respEvent <$> issueRequest
      | otherwise    = do
      maybeResp <- T.timeout (truncate (timeout * 1e6)) issueRequest
      case maybeResp of
        Just resp -> return (respEvent resp)
        _         -> closeManager mgr >> return Timeout

respEvent :: Response -> Event
respEvent resp =
    HttpResponse {
      respCode = statusCode resp
    , respContentLength = fromIntegral . L.length . responseBody $ resp
    }

analyseFull :: V.Vector Summary -> IO (Analysis SampleAnalysis)
analyseFull sums = do
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

analyseBasic :: V.Vector Summary -> Analysis Basic
analyseBasic sums = Analysis {
                      latency = Basic {
                                  mean = S.mean . G.map summElapsed $ sums
                                , stdDev = S.stdDev . G.map summElapsed $ sums
                                }
                    , latency99 = weightedAvg 99 100 . G.map summElapsed $ sums
                    , latency999 = weightedAvg 999 1000 . G.map summElapsed $ sums
                    , throughput = Basic {
                                     mean = S.mean slices / timeSlice
                                   , stdDev = S.stdDev slices / timeSlice
                                   }
                    , throughput10 = (/ timeSlice) . weightedAvg 10 100 $ slices
                    }
 where sumv = sortBy (compare `on` summStart) sums
       start = summStart . G.head $ sumv
       end = summEnd . G.last $ sumv
       elapsed = end - start
       timeSlice = min elapsed 1 / 200
       slices = U.unfoldrN (round (elapsed / timeSlice)) go (sumv,1)
         where go (v,i) = let (a,b) = G.span (\s -> summStart s <= t) v
                              t = start + (i * timeSlice)
                          in Just (fromIntegral $ G.length a,(b,i+1))

-- | Sort a vector.
sortBy :: (G.Vector v e) => I.Comparison e -> v e -> v e
sortBy cmp = G.modify (I.sortBy cmp)
{-# INLINE sortBy #-}
