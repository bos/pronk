{-# LANGUAGE OverloadedStrings, RecordWildCards, RelaxedPolyRec #-}

module Network.HTTP.LoadTest.Report
    (
      reportBasic
    , reportEvents
    , reportFull
    , writeReport
    -- * Other reports
    , csvEvents
    -- * Helper functions
    , buildTime
    ) where

import Control.Monad (forM_)
import Criterion.Analysis (SampleAnalysis(..), OutlierEffect(..),
                           OutlierVariance(..))
import Data.Data (Data)
import Data.List (sort)
import Data.Monoid (mappend, mconcat, mempty)
import Data.Text (Text)
import Data.Text.Buildable (build)
import Data.Text.Format (prec, shortest)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector (Vector)
import Network.HTTP.LoadTest.Types (Analysis(..), Basic(..), Event(..),
                                    Summary(..))
import Paths_pronk (getDataFileName)
import Prelude hiding (print)
import Statistics.Resampling.Bootstrap (Estimate(..))
import System.IO (Handle)
import System.IO.Unsafe (unsafePerformIO)
import Text.Hastache (MuType(..))
import Text.Hastache.Context (mkGenericContext)
import qualified Criterion.Report as R
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Text.Format as T
import qualified Data.Vector.Generic as G
import qualified Text.Hastache as H

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
  effect h (anOutlierVar latency)
  print "    99%:     {}\n    99.9%:   {}\n" (time latency99, time latency999)
  print "\nthroughput:\n" ()
  print "    mean:    {} req/sec\n" [estPoint (anMean throughput)]
  whenLoud $ do
    print "      lower: {}\n" [rate (estLowerBound (anMean throughput))]
    print "      upper: {}\n" [rate (estUpperBound (anMean throughput))]
  print "    std dev: {}\n" [rate (estPoint (anStdDev throughput))]
  whenLoud $ do
    print "      lower: {}\n" [rate (estLowerBound (anStdDev throughput))]
    print "      upper: {}\n" [rate (estUpperBound (anStdDev throughput))]
  effect h (anOutlierVar throughput)
  print "    10%:     {}\n" [rate throughput10]

time :: Double -> Builder
time = buildTime 4

rate :: Double -> Builder
rate r = prec 4 r `mappend` " req/sec"

buildTime :: Int -> Double -> Builder
buildTime precision t
     | t < 1e-3  = prec precision (t * 1e6) `mappend` " usec"
     | t < 1     = prec precision (t * 1e3) `mappend` " msec"
     | otherwise = prec precision t `mappend` " sec"

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

reportEvents :: Handle -> Vector Summary -> IO ()
reportEvents h sumv = do
  let evtMap = G.foldl' go H.empty . G.map summEvent $ sumv
      go m e = H.insertWith (+) (classify e) (1::Int) m
      classify Timeout          = 0
      classify HttpResponse{..} = respCode
  T.hprint h "responses:\n" ()
  forM_ (sort . H.toList $ evtMap) $ \(e,n) -> do
    let nameOf 0 = "timeout "
        nameOf k = "HTTP " `mappend` build k
    T.hprint h "    {} {}\n" (nameOf e, T.left 7 ' ' n)
  T.hprint h "\n" ()

csvEvents :: Vector Summary -> Builder
csvEvents sums = "start,elapsed,event\n" `mappend` G.foldr go mempty sums
  where
    firstStart = summStart (G.head sums)
    go Summary{..} b = mconcat [
                         shortest $ summStart - firstStart
                       , ","
                       , shortest summElapsed
                       , ","
                       , classify summEvent
                       , "\n"
                       ] `mappend` b
    classify Timeout          = "timeout"
    classify HttpResponse{..} = build respCode

-- | The path to the template and other files used for generating
-- reports.
templateDir :: FilePath
templateDir = unsafePerformIO $ getDataFileName "templates"
{-# NOINLINE templateDir #-}

writeReport :: (Data a) => FilePath -> Handle -> Analysis a -> IO ()
writeReport template h a@Analysis{..} = do
  let context "include" = MuLambdaM $
                          R.includeFile [templateDir, R.templateDir]
      context "latValues" = MuList . map mkGenericContext . G.toList $ lats
      context "thrValues" = R.vector "x" thrValues
      context n = mkGenericContext a n
      lats = G.map (\s -> s { summStart = summStart s - t }) latValues
          where t = summStart . G.head $ latValues
  tpl <- R.loadTemplate [".",templateDir] template
  bs <- H.hastacheStr H.defaultConfig tpl context
  L.hPutStr h bs
