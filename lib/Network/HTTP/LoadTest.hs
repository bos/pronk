{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Network.HTTP.LoadTest
    (
    -- * Running a load test
      NetworkError(..)
    , Config(..)
    , Req(..)
    , defaultConfig
    , run
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception.Lifted (catch, throwIO, try)
import Control.Monad (forM_, replicateM, when)
import Data.Either (partitionEithers)
import Data.List (nub)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Network.HTTP.Conduit
import Network.HTTP.LoadTest.Types
import Prelude hiding (catch)
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified System.Timeout.Lifted as T
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Network.HTTP.Types as H

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
    liftIO . writeChan ch =<< try (client cfg' mgr interval)
  (errs,vs) <- partitionEithers <$> replicateM concurrency (readChan ch)
  return $ case errs of
             [] -> Right . G.modify I.sort . V.concat $ vs
             _  -> Left (nub errs)

client :: Config -> Manager -> POSIXTime
       -> ResourceT IO (V.Vector Summary)
client Config{..} mgr interval = loop 0 []
  where
    loop !n acc
        | n == numRequests = return (V.fromList acc)
        | otherwise = do
      now <- liftIO getPOSIXTime
      !evt <- timedRequest
      now' <- liftIO getPOSIXTime
      let elapsed = now' - now
          !s = Summary {
                 summEvent = evt
               , summElapsed = realToFrac elapsed
               , summStart = realToFrac now
               }
      when (elapsed < interval) $
        liftIO . threadDelay . truncate $ (interval - elapsed) * 1000000
      loop (n+1) (s:acc)
    issueRequest :: ResourceT IO (Response L.ByteString)
    issueRequest = httpLbs (fromReq request) mgr
                   `catch` (throwIO . NetworkError)
    timedRequest :: ResourceT IO Event
    timedRequest
      | timeout == 0 = respEvent <$> issueRequest
      | otherwise    = do
      maybeResp <- T.timeout (truncate (timeout * 1e6)) issueRequest
      case maybeResp of
        Just resp -> return (respEvent resp)
        _         -> return Timeout

respEvent :: Response L.ByteString -> Event
respEvent resp =
    HttpResponse {
      respCode = H.statusCode $ responseStatus resp
    , respContentLength = fromIntegral . L.length . responseBody $ resp
    }
