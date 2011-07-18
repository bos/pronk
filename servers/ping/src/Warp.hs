{-# LANGUAGE OverloadedStrings #-}
import Blaze.ByteString.Builder (fromByteString)
import Network.HTTP.Types (status200)
import Network.Wai (Response(ResponseBuilder))
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8000 $ const $ return $ ResponseBuilder
    status200
    [("Content-Type", "text/plain"), ("Content-Length", "4")]
    $ fromByteString "PONG"
