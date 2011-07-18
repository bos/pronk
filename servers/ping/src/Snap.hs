{-# LANGUAGE OverloadedStrings #-}
import Snap.Http.Server (httpServe)
import Snap.Http.Server.Config (defaultConfig, setAccessLog, setErrorLog, setPort)
import Snap.Types (Snap, writeBS)

site :: Snap ()
site = writeBS "PONG"

main :: IO ()
main = do
  let config = setPort 8000 .
               setAccessLog Nothing .
               setErrorLog Nothing $
               defaultConfig
  httpServe config site
