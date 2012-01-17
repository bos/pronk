{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable, OverloadedStrings,
    RecordWildCards, ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.DeepSeq (rnf)
import Control.Exception (bracket, catch, evaluate, finally)
import Control.Monad (forM_, unless)
import Data.Aeson ((.=), encode, object)
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.LoadTest (NetworkError(..), Req(..))
import Network.HTTP.LoadTest.Analysis (analyseBasic, analyseFull)
import Network.HTTP.LoadTest.Environment (environment)
import Network.HTTP.LoadTest.Report
import Network.Socket (withSocketsDo)
import Prelude hiding (catch)
import System.CPUTime (getCPUTime)
import System.Console.CmdArgs
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (Handle, IOMode(..), hClose, hPutStrLn, openFile, stderr, stdout)
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Format as T
import qualified Data.Text.Lazy.IO as TL
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
    , dump_events :: Maybe FilePath
    , output :: Maybe FilePath
    , template :: FilePath
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
              , url = def &= argPos 0 &= typ "URL"

              -- --------------------------------------------------
              , from_file = def &= typ "FILE"
                &= groupname "Supplying a request body"
                &= help "Use file contents as request body"
              , literal = def &= typ "STRING"
                &= help "Use given text as request body"

              -- --------------------------------------------------
              , bootstrap = def
                &= groupname "Analysis of results"
                &= help "Statistically robust analysis of results"
              , dump_events = def &= typ "FILE"
                &= help "Save raw events in CSV format"
              , output = def &= typ "FILE"
                &= help "Write report to named file"
              , template = "report.tpl" &= typ "FILE"
                &= help "Use the given report template"
              , json = def &= typ "FILE"
                &= help "Save analysis in JSON format"
              } &= verbosity
                &= summary ("Pronk " ++ VERSION_pronk ++
                            " - a modern HTTP load tester")

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
  as@Args{..} <- cmdArgs $ defaultArgs &= program "pronk"
  validateArgs as
  cfg <- fromArgs as <$> createRequest as
  run <- timed "tested" $ LoadTest.run cfg
  case run of
    Left [NetworkError err] -> fatal (show err)
    Left errs -> do
      T.hprint stderr "Errors:\n" ()
      forM_ errs $ \(NetworkError err) -> T.hprint stderr "  {}\n" [show err]
      exitWith (ExitFailure 1)
    Right results -> do
      whenNormal $ T.print "analysing results\n" ()
      analysis <- timed "analysed" $ do
                    r <- if bootstrap
                         then Right <$> analyseFull results
                         else return . Left . analyseBasic $ results
                    evaluate $ rnf r
                    return r
      env <- environment
      let dump = object [ "config" .= cfg
                        , "environment" .= env
                        , "analysis" .= G.toJSON analysis ]
      maybeWriteFile json $ \h -> BL.hPut h (BL.append (encode dump) "\n")
      maybeWriteFile dump_events $ \h ->
          TL.hPutStr h . toLazyText . csvEvents $ results
      maybeWriteFile output $ \h -> either (writeReport template h)
                                           (writeReport template h) analysis
      whenNormal $ do
        reportEvents stdout results
        either (reportBasic stdout) (reportFull whenLoud stdout)
               analysis

maybeWriteFile :: Maybe FilePath -> (Handle -> IO ()) -> IO ()
maybeWriteFile (Just "-") act = act stdout
maybeWriteFile (Just p)   act = bracket (openFile p WriteMode) hClose act
maybeWriteFile _          _   = return ()

validateArgs :: Args -> IO ()
validateArgs Args{..} = do
  let p !? what | p         = Nothing
                | otherwise = Just $ "Argument to " ++ what
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
  req0 <- E.parseUrl url `catch` \(e::E.HttpException) ->
          fatal $ "could not parse URL - " ++
                case e of
                  E.InvalidUrlException _ s -> map toLower s
                  _ -> show e
  let check Nothing       = return "POST"
      check (Just "POST") = return "POST"
      check (Just "PUT")  = return "PUT"
      check _      = fatal "only POST or PUT may have a body"
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

timed :: Text -> IO a -> IO a
timed desc act = do
  startCPU <- getCPUTime
  startWall <- getPOSIXTime
  act `finally` do
    endCPU <- getCPUTime
    endWall <- getPOSIXTime
    let elapsedCPU  = fromIntegral (endCPU - startCPU) / 1e12
        elapsedWall = realToFrac $ endWall - startWall
        ratio = elapsedCPU / elapsedWall
    whenNormal $
      -- Try to work around the 64-bit Mac getCPUTime bug
      -- http://hackage.haskell.org/trac/ghc/ticket/4970
      if ratio > 0 && ratio < 32
      then T.print "{} in {} ({}% CPU)\n"
               (desc, buildTime 4 elapsedWall,
                T.fixed 1 $ 100 * elapsedCPU / elapsedWall)
      else T.print "{} in {}\n"
               (desc, buildTime 4 elapsedWall)

fatal :: String -> IO a
fatal e = do
  T.hprint stderr "Error: {}\n" (T.Only e)
  exitWith (ExitFailure 1)
