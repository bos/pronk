import Snap.Extension.Server
import Application
import Site

main :: IO ()
main = quickHttpServe applicationInitializer site
