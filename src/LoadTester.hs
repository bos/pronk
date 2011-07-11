{-# LANGUAGE BangPatterns, DeriveDataTypeable, RecordWildCards #-}

module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.Time.Clock.POSIX
import Network.HTTP.Enumerator
import Network.Socket (withSocketsDo)
import System.Console.CmdArgs

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

main :: IO ()
main = withSocketsDo $ do
  Args{..} <- cmdArgs defaultArgs
  req <- parseUrl url
  let !interval | requests_per_second == 0 = 0
                | otherwise = 1 / fromIntegral requests_per_second
  withManager $ \mgr -> do
    let loop !n now
            | n == num_requests = return ()
            | otherwise = do
          httpLbs req mgr
          now' <- getPOSIXTime
          let elapsed = now' - now
          when (elapsed < interval) $
            threadDelay . truncate $ (interval - elapsed) * 1000000
          loop (n+1) =<< getPOSIXTime
    loop 0 =<< getPOSIXTime
