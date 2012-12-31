{-# OPTIONS_GHC -fsimpl-tick-factor=150 #-}
{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Network.HTTP.LoadTest.Analysis
    (
    -- * Result analysis
      Analysis(..)
    , Basic(..)
    , analyseBasic
    , analyseFull
    ) where

import Criterion.Analysis (SampleAnalysis, analyseSample)
import Network.HTTP.LoadTest.Types (Analysis(..), Basic(..), Summary(..))
import Prelude hiding (catch)
import Statistics.Quantile (weightedAvg)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Statistics.Sample as S

analyseFull :: V.Vector Summary -> Double -> IO (Analysis SampleAnalysis)
analyseFull sumv elapsed = do
  let ci = 0.95
      resamples = 10 * 1000
  l <- analyseSample ci (G.convert . G.map summElapsed $ sumv) resamples
  return Analysis {
                 latency = l
               , latency99 = weightedAvg 99 100 . G.map summElapsed $ sumv
               , latency999 = weightedAvg 999 1000 . G.map summElapsed $ sumv
               , latValues = sumv
               , throughput = fromIntegral (G.length sumv) / elapsed
    }

analyseBasic :: V.Vector Summary -> Double -> Analysis Basic
analyseBasic sumv elapsed = Analysis {
                      latency = Basic {
                                  mean = S.mean . G.map summElapsed $ sumv
                                , stdDev = S.stdDev . G.map summElapsed $ sumv
                                }
                    , latency99 = weightedAvg 99 100 . G.map summElapsed $ sumv
                    , latency999 = weightedAvg 999 1000 . G.map summElapsed $ sumv
                    , latValues = sumv
                    , throughput = fromIntegral (G.length sumv) / elapsed
                    }
