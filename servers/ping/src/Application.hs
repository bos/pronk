module Application
  ( Application
  , applicationInitializer
  ) where

import           Snap.Extension

type Application = SnapExtend ApplicationState

type ApplicationState = ()

applicationInitializer :: Initializer ApplicationState
applicationInitializer = return ()
