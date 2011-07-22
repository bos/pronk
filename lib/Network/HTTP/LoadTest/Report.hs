{-# LANGUAGE OverloadedStrings, RecordWildCards, RelaxedPolyRec #-}

module Network.HTTP.LoadTest.Report
    (
      reportBasic
    , reportFull
    ) where

import Criterion.Analysis (SampleAnalysis(..), OutlierEffect(..),
                           OutlierVariance(..))
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Text.Buildable (build)
import Data.Text.Lazy.Builder (Builder)
import Network.HTTP.LoadTest.Types (Analysis(..), Basic(..))
import Prelude hiding (print)
import Statistics.Resampling.Bootstrap (Estimate(..))
import System.IO (Handle)
import qualified Data.Text.Format as T

reportBasic :: Handle -> Analysis Basic -> IO ()
reportBasic h Analysis{..} = do
  let print a b = T.hprint h a b
  print "latency:\n" ()
  print "    mean:    {}\n" [time (mean latency)]
  print "    std dev: {}\n" [time (stdDev latency)]
  print "    99%:     {}\n    99.9%:   {}\n" (time latency99, time latency999)
  print "\nthroughput:\n" ()
  print "    mean:    {} req/sec\n" [mean throughput]
  print "    std dev: {} req/sec\n" [stdDev throughput]
  print "    10%:     {} req/sec\n" [throughput10]

reportFull :: (IO () -> IO ()) -> Handle -> Analysis SampleAnalysis -> IO ()
reportFull whenLoud h Analysis{..} = do
  let print a b = T.hprint h a b
  print "latency:\n" ()
  print "    mean:    {}\n" [time (estPoint (anMean latency))]
  whenLoud $ do
    print "      lower: {}\n" [time (estLowerBound (anMean latency))]
    print "      upper: {}\n" [time (estUpperBound (anMean latency))]
  print "    std dev: {}\n" [time (estPoint (anStdDev latency))]
  whenLoud $ do
    print "      lower: {}\n" [time (estLowerBound (anStdDev latency))]
    print "      upper: {}\n" [time (estUpperBound (anStdDev latency))]
  effect h (anOutliers latency)
  print "    99%:     {}\n    99.9%:   {}\n" (time latency99, time latency999)
  print "\nthroughput:\n" ()
  print "    mean:    {} req/sec\n" [estPoint (anMean throughput)]
  whenLoud $ do
    print "      lower: {} req/sec\n" [estLowerBound (anMean throughput)]
    print "      upper: {} req/sec\n" [estUpperBound (anMean throughput)]
  print "    std dev: {} req/sec\n" [estPoint (anStdDev throughput)]
  whenLoud $ do
    print "      lower: {} req/sec\n" [estLowerBound (anStdDev throughput)]
    print "      upper: {} req/sec\n" [estUpperBound (anStdDev throughput)]
  effect h (anOutliers throughput)
  print "    10%:     {} req/sec\n" [throughput10]

time :: Double -> Builder
time t
     | t < 1e-3  = build (t * 1e6) `mappend` " usec"
     | t < 1     = build (t * 1e3) `mappend` " msec"
     | otherwise = build t `mappend` " sec"

effect :: Handle -> OutlierVariance -> IO ()
effect h OutlierVariance{..} =
    case ovEffect of
      Unaffected -> return ()
      _ -> T.hprint h "    estimates {} affected by outliers ({}%)\n"
           (howMuch, T.fixed 1 (ovFraction * 100))
    where howMuch = case ovEffect of
                      Unaffected -> "not" :: Text
                      Slight     -> "slightly"
                      Moderate   -> "moderately"
                      Severe     -> "severely"
