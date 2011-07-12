{-# LANGUAGE BangPatterns, DeriveDataTypeable, RecordWildCards,
    ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (getChanContents, newChan, writeChan)
import Control.Exception (IOException, catch)
import Control.Monad (forM_, unless, when)
import Data.Maybe (catMaybes)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Enumerator
import Network.Socket (withSocketsDo)
import Prelude hiding (catch)
import System.Console.CmdArgs
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy as L
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
              , timeout = def
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
    , summTime :: {-# UNPACK #-} !Double
    } deriving (Eq, Show)

main :: IO ()
main = withSocketsDo $ do
  as@Args{..} <- cmdArgs defaultArgs
  validateArgs as
  req <- parseUrl url
  let reqs = zipWith (+) (replicate concurrency reqsPerThread)
                         (replicate leftover 1 ++ repeat 0)
        where (reqsPerThread,leftover) = num_requests `quotRem` concurrency
  let !interval | requests_per_second == 0 = 0
                | otherwise = realToFrac (1 / requests_per_second)
  ch <- newChan
  forM_ reqs $ \numReqs ->
    forkIO . withManager $ \mgr -> do
      let issueRequest = httpLbs req mgr
          timedRequest
            | timeout == 0 = respEvent <$> issueRequest
            | otherwise    = do
            maybeResp <- T.timeout (truncate (timeout * 1e6)) issueRequest
            case maybeResp of
              Just resp -> return (respEvent resp)
              _         -> closeManager mgr >> return Timeout
          loop !n now
              | n == numReqs = return ()
              | otherwise = do
            !evt <- timedRequest `catch`
                    \(_::IOException) -> closeManager mgr >> return NetworkError
            now' <- getPOSIXTime
            let elapsed = now' - now
            writeChan ch Summary {
                            summEvent = evt
                          , summTime = realToFrac elapsed
                          }
            when (elapsed < interval) $
              threadDelay . truncate $ (interval - elapsed) * 1000000
            loop (n+1) =<< getPOSIXTime
      loop 0 =<< getPOSIXTime
  results <- take num_requests <$> getChanContents ch
  forM_ results print

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
