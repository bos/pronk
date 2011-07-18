{-# LANGUAGE BangPatterns, DeriveDataTypeable, OverloadedStrings,
    RecordWildCards #-}

module Main (main) where

import Control.Monad (forM_, unless)
import Criterion.Analysis (SampleAnalysis(..), OutlierEffect(..),
                           OutlierVariance(..))
import Data.Maybe (catMaybes)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Text.Buildable (build)
import Data.Text.Lazy.Builder (Builder)
import Network.HTTP.LoadTest (Analysis(..), NetworkError(..))
import Network.Socket (withSocketsDo)
import Statistics.Resampling.Bootstrap (Estimate(..))
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
              } &= verbosity

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
  run <- LoadTest.run (fromArgs as)
  case run of
    Left [NetworkError err] ->
      T.hprint stderr "Error: {}" [show err] >> exitWith (ExitFailure 1)
    Left errs -> do
      T.hprint stderr "Errors:\n" ()
      forM_ errs $ \(NetworkError err) -> T.hprint stderr "  {}\n" [show err]
      exitWith (ExitFailure 1)
    Right results -> do
      whenNormal $ T.print "analysing results\n" ()
      report =<< LoadTest.analyse results

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

report :: Analysis -> IO ()
report Analysis{..} = do
  T.print "latency:\n" ()
  T.print "    mean:    {}\n" [time (estPoint (anMean latency))]
  whenLoud $ do
    T.print "      lower: {}\n" [time (estLowerBound (anMean latency))]
    T.print "      upper: {}\n" [time (estUpperBound (anMean latency))]
  T.print "    std dev: {}\n" [time (estPoint (anStdDev latency))]
  whenLoud $ do
    T.print "      lower: {}\n" [time (estLowerBound (anStdDev latency))]
    T.print "      upper: {}\n" [time (estUpperBound (anStdDev latency))]
  effect (anOutliers latency)
  T.print "    99%:     {}\n    99.9%:   {}\n" (time latency99, time latency999)
  T.print "\nthroughput:\n" ()
  T.print "    mean:    {} req/sec\n" [estPoint (anMean throughput)]
  whenLoud $ do
    T.print "      lower: {} req/sec\n" [estLowerBound (anMean throughput)]
    T.print "      upper: {} req/sec\n" [estUpperBound (anMean throughput)]
  T.print "    std dev: {} req/sec\n" [estPoint (anStdDev throughput)]
  whenLoud $ do
    T.print "      lower: {} req/sec\n" [estLowerBound (anStdDev throughput)]
    T.print "      upper: {} req/sec\n" [estUpperBound (anStdDev throughput)]
  effect (anOutliers throughput)
  T.print "    10%:     {} req/sec\n" [throughput10]

time :: Double -> Builder
time t
     | t < 1e-3  = build (t * 1e6) `mappend` " usec"
     | t < 1     = build (t * 1e3) `mappend` " msec"
     | otherwise = build t `mappend` " sec"

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
