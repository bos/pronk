{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards, RelaxedPolyRec, ViewPatterns #-}

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
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend, mconcat, mempty)
import Data.Text (Text)
import Data.Text.Buildable (build)
import Data.Text.Format (prec, shortest)
import Data.Text.Lazy.Builder (Builder)
import Data.Vector (Vector)
import Network.HTTP.LoadTest.Types (Analysis(..), Basic(..), Event(..),
                                    Summary(..), summEnd)
import Paths_pronk (getDataFileName)
import Prelude hiding (print)
import Statistics.Function (sort)
import Statistics.Resampling.Bootstrap (Estimate(..))
import Statistics.Sample.KernelDensity (kde)
import System.IO (Handle)
import System.IO.Unsafe (unsafePerformIO)
import Text.Hastache (MuType(..))
import Text.Hastache.Context (mkGenericContext)
import qualified Criterion.Report as R
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.List as List
import qualified Data.MeldableHeap as Q
import qualified Data.Text.Format as T
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Text.Hastache as H

reportBasic :: Handle -> Analysis Basic -> IO ()
reportBasic h Analysis{..} = do
  let print a b = T.hprint h a b
  print "latency:\n" ()
  print "    mean:    {}\n" [time (mean latency)]
  print "    std dev: {}\n" [time (stdDev latency)]
  print "    99%:     {}\n    99.9%:   {}\n" (time latency99, time latency999)
  print "\nthroughput:  {}\n" [rate throughput]

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
  print "\nthroughput:  {}\n" [rate throughput]

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
  forM_ (List.sort . H.toList $ evtMap) $ \(e,n) -> do
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

writeReport :: (Data a) => FilePath -> Handle -> Double -> Analysis a -> IO ()
writeReport template h elapsed a@Analysis{..} = do
  let context "include" = return . MuLambdaM $
                          R.includeFile [templateDir, R.templateDir]
      context "elapsed"   = return $ MuVariable elapsed
      context "latKdeTimes" = return $ R.vector "x" latKdeTimes
      context "latKdePDF" = return $ R.vector "x" latKdePDF
      context "latKde"    = return $
                            R.vector2 "time" "pdf" latKdeTimes latKdePDF
      context "latValues" = return . MuList
                            . map mkGenericContext . G.toList $ lats
      context "thrTimes" = return $ R.vector "x" thrTimes
      context "thrValues" = return $ R.vector "x" thrValues
      context "concTimes" = return . R.vector "x" . U.fromList $ map fstS conc
      context "concValues" = return . R.vector "x" . U.fromList $ map sndS conc
      context n = mkGenericContext a n
      (latKdeTimes,latKdePDF) = kde 128 . G.convert . G.map summElapsed $ latValues
      lats = G.map (\s -> s { summStart = summStart s - t }) latValues
          where t = summStart . G.head $ latValues
      (thrTimes,thrValues) = graphThroughput (min (G.length latValues) 50) elapsed latValues
      conc = graphConcurrency lats
  tpl <- R.loadTemplate [".",templateDir] template
  bs <- H.hastacheStr H.defaultConfig tpl context
  L.hPutStr h bs

data T = T (U.Vector Double) {-# UNPACK #-} !Double

-- | Compute a graph of throughput, requests completed per time
-- interval.
graphThroughput :: Int          -- ^ Number of time slices.
                -> Double       -- ^ Amount of time elapsed.
                -> V.Vector Summary -> (U.Vector Double, U.Vector Double)
graphThroughput slices elapsed sumv =
    (G.generate slices $ \i -> fromIntegral i * timeSlice,
     G.unfoldrN slices go (T endv 0))
  where go (T v i) = Just (fromIntegral (G.length a), T b j)
           where (a,b) = G.span (<=t) v
                 t = start + (j * timeSlice)
                 j = i+1
        timeSlice = elapsed / fromIntegral slices
        start = summStart . G.head $ sumv
        endv = G.convert . sort . G.map summEnd $ sumv

data S = S {
      fstS :: {-# UNPACK #-} !Double
    , sndS :: {-# UNPACK #-} !Int
    }

-- | Compute a graph of concurrency.
graphConcurrency :: V.Vector Summary -> [S]
graphConcurrency = scanl1 f . filter ((/=0) . sndS) . map (foldl1 (flip f)) .
                   List.groupBy ((==) `on` fstS) . go Q.empty . G.toList
  where
    f (S _ i) (S t j) = S t (i+j)
    go q es@(Summary{..}:xs)
        | summStart < t = S summStart 1 : go insQ xs
        | otherwise     = S t (-1)      : go delQ es
      where (t,delQ) = fromMaybe (1e300,q) $ Q.extractMin q
            insQ     = Q.insert (summStart+summElapsed) q
    go q _ = drain q
      where drain (Q.extractMin -> Just (t,q')) = S t (-1) : drain q'
            drain _ = []
