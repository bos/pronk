{-# LANGUAGE BangPatterns, DeriveDataTypeable, OverloadedStrings,
    RecordWildCards #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, unless)
import Criterion.Analysis (SampleAnalysis(..), OutlierEffect(..),
                           OutlierVariance(..))
import Data.Aeson ((.=), encode, object)
import Data.Maybe (catMaybes)
import Data.Monoid (mappend)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Buildable (build)
import Data.Text.Lazy.Builder (Builder)
import Network.HTTP.LoadTest (Analysis(..), Basic(..), NetworkError(..), Req(..))
import Network.Socket (withSocketsDo)
import Statistics.Resampling.Bootstrap (Estimate(..))
import System.Console.CmdArgs
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Format as T
import qualified Network.HTTP.Enumerator as E
import qualified Network.HTTP.LoadTest as LoadTest

data Args = Args {
      concurrency :: Int
    , method :: Maybe String
    , num_requests :: Int
    , requests_per_second :: Double
    , timeout :: Double
    , url :: String

    , from_file :: Maybe FilePath
    , literal :: Maybe String

    , bootstrap :: Bool
    , json :: Maybe FilePath
    } deriving (Eq, Show, Typeable, Data)

defaultArgs :: Args
defaultArgs = Args {
                concurrency = 1
                &= groupname "Load testing"
                &= help "Number of requests to issue concurrently"
              , method = def &= typ "METHOD"
                &= help "HTTP method to use (GET, POST, ...)"
              , num_requests = 1
                &= help "Total number of requests to issue"
              , requests_per_second = def
                &= help "Maximum request rate to sustain"
              , timeout = 60 &= typ "SECS"
                &= help "Time to wait before killing a connection"
              , url = def &= argPos 0

              , from_file = def &= typ "FILE"
                &= groupname "Supplying a request body"
                &= help "Use file contents as request body"
              , literal = def &= typ "STRING"
                &= help "Use given text as request body"

              , bootstrap = def
                &= groupname "Analysis of results"
                &= help "Statistically robust analysis of results"
              , json = def &= typ "FILE"
                &= help "Save analysis in JSON format"
              } &= verbosity

fromArgs :: Args -> E.Request IO -> LoadTest.Config
fromArgs Args{..} req =
    LoadTest.Config {
      LoadTest.concurrency = concurrency
    , LoadTest.numRequests = num_requests
    , LoadTest.requestsPerSecond = requests_per_second
    , LoadTest.timeout = timeout
    , LoadTest.request = Req req
    }

main :: IO ()
main = withSocketsDo $ do
  as@Args{..} <- cmdArgs $ defaultArgs &= program "http-load-tester"
  validateArgs as
  cfg <- fromArgs as <$> createRequest as
  run <- LoadTest.run cfg
  case run of
    Left [NetworkError err] ->
      T.hprint stderr "Error: {}" [show err] >> exitWith (ExitFailure 1)
    Left errs -> do
      T.hprint stderr "Errors:\n" ()
      forM_ errs $ \(NetworkError err) -> T.hprint stderr "  {}\n" [show err]
      exitWith (ExitFailure 1)
    Right results -> do
      whenNormal $ T.print "analysing results\n" ()
      analysis <- if bootstrap
                  then Right <$> LoadTest.analyseFull results
                  else return . Left . LoadTest.analyseBasic $ results
      let dump = object [ "config" .= cfg, "analysis" .= analysis ]
      case json of
        Just "-" -> L.putStrLn (encode dump)
        Just f   -> L.writeFile f (encode dump)
        _        -> return ()
      whenNormal $ either reportBasic reportFull analysis

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

createRequest :: Args -> IO (E.Request IO)
createRequest Args{..} = do
  req0 <- E.parseUrl url
  let check Nothing = return "POST"
      check (Just "POST") = return "POST"
      check (Just "PUT")  = return "PUT"
      check _      = do
        hPutStrLn stderr "Error: only POST or PUT may have a body"
        exitWith (ExitFailure 1)
  case (from_file, literal) of
    (Nothing,Nothing) -> return req0 { E.method = maybe "GET" B.pack method }
    (Just f,Nothing) -> do
      s <- B.readFile f
      meth <- check method
      return req0 { E.method = meth
                  , E.requestBody = E.RequestBodyBS s }
    (Nothing,Just s) -> do
      meth <- check method
      return req0 { E.method = meth
                  , E.requestBody = E.RequestBodyBS . encodeUtf8 . pack $ s
                  }
    _ -> do
      hPutStrLn stderr "Error: --literal and --from-file are mutually exclusive"
      exitWith (ExitFailure 1)

reportBasic :: Analysis Basic -> IO ()
reportBasic Analysis{..} = do
  T.print "latency:\n" ()
  T.print "    mean:    {}\n" [time (mean latency)]
  T.print "    std dev: {}\n" [time (stdDev latency)]
  T.print "    99%:     {}\n    99.9%:   {}\n" (time latency99, time latency999)
  T.print "\nthroughput:\n" ()
  T.print "    mean:    {} req/sec\n" [mean throughput]
  T.print "    std dev: {} req/sec\n" [stdDev throughput]
  T.print "    10%:     {} req/sec\n" [throughput10]


reportFull :: Analysis SampleAnalysis -> IO ()
reportFull Analysis{..} = do
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
