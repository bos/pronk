{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards,
    ScopedTypeVariables, ViewPatterns #-}

module Network.HTTP.LoadTest.Types
    (
    -- * Running a load test
      Config(..)
    , Req(..)
    , defaultConfig
    , NetworkError(..)
    -- * Results
    , Event(..)
    , Summary(..)
    , summEnd
    -- * Result analysis
    , Analysis(..)
    , Basic(..)
    ) where

import Control.Applicative ((<$>), (<*>), pure, empty)
import Control.Arrow (first)
import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception, IOException, SomeException, try)
import Data.Aeson.Types (Value(..), FromJSON(..), ToJSON(..), (.:), (.=), object)
import Data.Bits (xor)
import Data.Data (Data)
import Data.Hashable (Hashable(hash))
import Data.Typeable (Typeable)
import Network.HTTP.Enumerator (Request(..), parseUrl)
import Network.HTTP.Types (renderQuery)
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

newtype Req = Req {
      fromReq :: Request IO
    } deriving (Typeable)

instance Show Req where
    show (Req req) = concatMap B.unpack
                         [ http, host req, portie, path req
                         , renderQuery True $ queryString req ]
        where http | secure req = "https://"
                   | otherwise  = "http://"
              isDefaultPort | secure req = port req == 443
                            | otherwise  = port req == 80
              portie | isDefaultPort = ""
                     | otherwise     = B.pack $ ":" ++ show (port req)

instance ToJSON Req where
    toJSON req@(Req req') = toJSON [ "url"     .= show req
                                   , "method"  .= method req'
                                   , "headers" .= headers req'
                                   ]
        where headers = map (first CI.original) . requestHeaders

instance FromJSON Req where
    parseJSON (Object v) = do
      (u,m,h) <- (,,) <$> (v .: "url") <*> (v .: "method") <*> (v .: "headers")
      req <- unsafePerformIO $ do
               t <- try $ parseUrl (T.unpack u)
               return $ case t of
                          Left (_::SomeException) -> empty
                          Right r -> return r
      return . Req $ req {
                        method = m
                      , requestHeaders = map (first CI.mk) h
                      }
    parseJSON _ = empty

data Config = Config {
      concurrency :: Int
    , numRequests :: Int
    , requestsPerSecond :: Double
    , timeout :: Double
    , request :: Req
    } deriving (Show, Typeable)

instance ToJSON Config where
    toJSON Config{..} = object [
                          "concurrency" .= concurrency
                        , "numRequests" .= numRequests
                        , "requestsPerSecond" .= requestsPerSecond
                        , "timeout" .= timeout
                        , "request" .= request
                        ]

instance FromJSON Config where
    parseJSON (Object v) = Config <$>
                           v .: "concurrency" <*>
                           v .: "numRequests" <*>
                           v .: "requestsPerSecond" <*>
                           v .: "timeout" <*>
                           v .: "request"
    parseJSON _ = empty

emptyReq :: Req
emptyReq = Req . unsafePerformIO $ parseUrl "http://127.0.0.1/"
{-# NOINLINE emptyReq #-}

defaultConfig :: Config
defaultConfig = Config {
                concurrency = 1
              , numRequests = 1
              , requestsPerSecond = 0
              , timeout = 60
              , request = emptyReq
              }

data Event =
    HttpResponse {
      respCode :: {-# UNPACK #-} !Int
    , respContentLength :: {-# UNPACK #-} !Int
    } | Timeout
    deriving (Eq, Ord, Read, Show, Typeable, Data)

instance Hashable Event where
    hash Timeout = 0
    hash HttpResponse{..} = respCode `xor` respContentLength

instance ToJSON Event where
    toJSON HttpResponse{..} = toJSON (respCode, respContentLength)
    toJSON Timeout          = "timeout"

instance FromJSON Event where
    parseJSON (Array (G.toList -> [Number c,Number l])) =
        pure $ HttpResponse (truncate c) (truncate l)
    parseJSON (String "timeout") = pure Timeout
    parseJSON _ = empty

-- | Exception thrown if issuing a HTTP request fails.
data NetworkError = NetworkError {
      fromNetworkError :: IOException
    } deriving (Eq, Show, Typeable)

instance Exception NetworkError

data Summary = Summary {
      summStart :: {-# UNPACK #-} !Double
    , summElapsed :: {-# UNPACK #-} !Double
    , summEvent :: Event
    } deriving (Eq, Ord, Read, Show, Typeable, Data)

summEnd :: Summary -> Double
summEnd Summary{..} = summStart + summElapsed

instance ToJSON Summary where
    toJSON Summary{..} = object [
                           "start" .= summStart
                         , "elapsed" .= summElapsed
                         , "event" .= summEvent
                         ]
                                  
instance FromJSON Summary where
    parseJSON (Object v) = Summary <$>
                           v .: "start" <*>
                           v .: "elapsed" <*>
                           v .: "event"
    parseJSON _ = empty

data Analysis a = Analysis {
      latency :: !a
    , latency99 :: !Double
    , latency999 :: !Double
    , latValues :: V.Vector Summary
    , throughput :: !a
    , throughput10 :: !Double
    , thrValues :: U.Vector Double
    } deriving (Eq, Show, Typeable, Data)

instance (NFData a) => NFData (Analysis a) where
    rnf Analysis{..} = rnf latency `seq` rnf throughput

data Basic = Basic {
      mean :: {-# UNPACK #-} !Double
    , stdDev :: {-# UNPACK #-} !Double
    } deriving (Eq, Show, Typeable, Data)

instance NFData Basic

instance ToJSON Basic where
    toJSON Basic{..} = object [
                         "mean" .= mean
                       , "stdDev" .= stdDev
                       ]

instance FromJSON Basic where
    parseJSON (Object v) = Basic <$>
                           v .: "mean" <*>
                           v .: "stdDev"
    parseJSON _ = empty

instance (ToJSON a) => ToJSON (Analysis a) where
    toJSON Analysis{..} = object [
                            "latency" .= latency
                          , "latency99" .= latency99
                          , "latency999" .= latency999
                          , "latValues" .= latValues
                          , "throughput" .= throughput
                          , "throughput10" .= throughput10
                          , "thrValues" .= thrValues
                          ]

instance (FromJSON a) => FromJSON (Analysis a) where
    parseJSON (Object v) = Analysis <$>
                           v .: "latency" <*>
                           v .: "latency99" <*>
                           v .: "latency999" <*>
                           v .: "latValues" <*>
                           v .: "throughput" <*>
                           v .: "throughput10" <*>
                           v .: "thrValues"
    parseJSON _ = empty
