{-# LANGUAGE BangPatterns, DeriveDataTypeable, RecordWildCards #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad (forM_, replicateM, when)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Enumerator
import Network.Socket (withSocketsDo)
import System.Console.CmdArgs
import qualified Data.ByteString.Lazy as L

data Args = Args {
      concurrency :: Int
    , num_requests :: Int
    , requests_per_second :: Int
    , url :: String
    } deriving (Eq, Show, Typeable, Data)

defaultArgs :: Args
defaultArgs = Args {
                concurrency = 1
              , num_requests = 1
              , requests_per_second = def
              , url = def &= argPos 0
              }

data Summary = Summary {
      respCode :: {-# UNPACK #-} !Int
    , respLength :: {-# UNPACK #-} !Int
    , respTime :: {-# UNPACK #-} !Double
    } deriving (Show)

main :: IO ()
main = withSocketsDo $ do
  Args{..} <- cmdArgs defaultArgs
  req <- parseUrl url
  let (reqsPerThread,slop) = num_requests `quotRem` concurrency
      reqs = zipWith (+) (replicate concurrency reqsPerThread)
                         (replicate slop 1 ++ repeat 0)
  let !interval | requests_per_second == 0 = 0
                | otherwise = 1 / fromIntegral requests_per_second
  ch <- newChan
  forM_ reqs $ \numReqs ->
    forkIO . withManager $ \mgr -> do
      let loop ss !n now
              | n == numReqs = return ss
              | otherwise = do
            resp <- httpLbs req mgr
            now' <- getPOSIXTime
            let elapsed = now' - now
                !s = Summary {
                       respCode = statusCode resp
                     , respLength = fromIntegral . L.length . responseBody $ resp
                     , respTime = realToFrac elapsed
                     }
            when (elapsed < interval) $
              threadDelay . truncate $ (interval - elapsed) * 1000000
            loop (s:ss) (n+1) =<< getPOSIXTime
      writeChan ch =<< loop [] 0 =<< getPOSIXTime
  results <- concat `fmap` (replicateM concurrency $ readChan ch)
  print results

