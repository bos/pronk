{-# LANGUAGE BangPatterns, DeriveDataTypeable, OverloadedStrings,
    RecordWildCards #-}

module Main (main) where

import Control.Monad (forM_, unless)
import Data.Maybe (catMaybes)
import Network.Socket (withSocketsDo)
import Prelude hiding (catch)
import System.Console.CmdArgs
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import qualified Data.Text.Format as T
import qualified Network.HTTP.LoadTest as LoadTest

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

fromArgs :: Args -> LoadTest.Config
fromArgs Args{..} = LoadTest.Config {
                      LoadTest.concurrency = concurrency
                    , LoadTest.numRequests = num_requests
                    , LoadTest.requestsPerSecond = requests_per_second
                    , LoadTest.timeout = timeout
                    , LoadTest.url = url
                    }

main :: IO ()
main = withSocketsDo $ do
  as@Args{..} <- cmdArgs defaultArgs
  validateArgs as
  !results <- LoadTest.run (fromArgs as)
  T.print "analysing results\n" ()
  analysis <- LoadTest.analyse results
  LoadTest.report analysis

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
