{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}

module Network.HTTP.LoadTest.Environment
    (
      Environment(..)
    , environment
    ) where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson.Types (Value(..), FromJSON(..), ToJSON(..), (.:), (.=), object)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Conc (numCapabilities)
import System.PosixCompat.Unistd (SystemID(..), getSystemID)

data Environment = Environment {
      osName :: String
    , osVersion :: String
    , hostName :: String
    , numCores :: Int
    } deriving (Eq, Read, Show, Typeable, Data)

instance ToJSON Environment where
    toJSON Environment{..} = object [
                               "osName" .= osName
                             , "osVersion" .= osVersion
                             , "hostName" .= hostName
                             , "numCores" .= numCores
                             ]

instance FromJSON Environment where
    parseJSON (Object v) = Environment <$>
                           v .: "osName" <*>
                           v .: "osVersion" <*>
                           v .: "hostName" <*>
                           v .: "numCores"
    parseJSON _ = empty

environment :: IO Environment
environment = do
  SystemID{..} <- getSystemID
  return Environment {
                osName = systemName
              , osVersion = version
              , hostName = nodeName
              , numCores = numCapabilities
              }
