{-# LANGUAGE StrictData #-}

module Config (AppConfig (..), AppM) where

import Control.Monad.Trans.Reader (ReaderT)
import Network.HTTP.Client (Manager)
import Servant (Handler)

data AppConfig = AppConfig
  { httpManager :: Manager,
    todosBaseUrl :: String
  }

type AppM = ReaderT AppConfig Handler
