{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

module Network.HTTP.LoadTest.Types
    (
    -- * Running a load test
      Config(..)
    , defaultConfig
    , NetworkError(..)
    -- * Results
    , Event(..)
    , Summary(..)
    , summEnd
    -- * Result analysis
    , Analysis(..)
    ) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Exception (Exception, IOException)
import Criterion.Analysis (SampleAnalysis(..))
import Data.Data (Data)
import Data.Aeson.Types (Value(Object), FromJSON(..), ToJSON(..), (.:), (.=), object)
import Data.Typeable (Typeable)

data Config = Config {
      concurrency :: Int
    , numRequests :: Int
    , requestsPerSecond :: Double
    , timeout :: Double
    , url :: String
    } deriving (Eq, Read, Show, Typeable, Data)

instance ToJSON Config where
    toJSON Config{..} = object [
                          "concurrency" .= concurrency
                        , "numRequests" .= numRequests
                        , "requestsPerSecond" .= requestsPerSecond
                        , "timeout" .= timeout
                        , "url" .= url
                        ]

instance FromJSON Config where
    parseJSON (Object v) = Config <$>
                           v .: "concurrency" <*>
                           v .: "numRequests" <*>
                           v .: "requestsPerSecond" <*>
                           v .: "timeout" <*>
                           v .: "url"
    parseJSON _ = empty

defaultConfig :: Config
defaultConfig = Config {
                concurrency = 1
              , numRequests = 1
              , requestsPerSecond = 0
              , timeout = 60
              , url = ""
              }

data Event =
    HttpResponse {
      respCode :: {-# UNPACK #-} !Int
    , respContentLength :: {-# UNPACK #-} !Int
    } | Timeout
    deriving (Eq, Read, Show, Typeable, Data)

-- | Exception thrown if issuing a HTTP request fails.
data NetworkError = NetworkError {
      fromNetworkError :: IOException
    } deriving (Eq, Show, Typeable)

instance Exception NetworkError

data Summary = Summary {
      summEvent :: Event
    , summElapsed :: {-# UNPACK #-} !Double
    , summStart :: {-# UNPACK #-} !Double
    } deriving (Eq, Read, Show, Typeable, Data)

summEnd :: Summary -> Double
summEnd Summary{..} = summStart + summElapsed

data Analysis = Analysis {
      latency :: !SampleAnalysis
    , latency99 :: !Double
    , latency999 :: !Double
    , throughput :: !SampleAnalysis
    , throughput10 :: !Double
    } deriving (Eq, Show, Typeable, Data)

instance ToJSON Analysis where
    toJSON Analysis{..} = object [
                            "latency" .= latency
                          , "latency99" .= latency99
                          , "latency999" .= latency999
                          , "throughput" .= throughput
                          , "throughput10" .= throughput10
                          ]

instance FromJSON Analysis where
    parseJSON (Object v) = Analysis <$>
                           v .: "latency" <*>
                           v .: "latency99" <*>
                           v .: "latency999" <*>
                           v .: "throughput" <*>
                           v .: "throughput10"
    parseJSON _ = empty
