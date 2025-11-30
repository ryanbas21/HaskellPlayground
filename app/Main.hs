{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Config (AppConfig (AppConfig), AppM)
import Control.Monad.Trans.Reader (runReaderT)
import Handlers (addPost, commentHandler, commentsHandlerById, deletePost, postsHandler, postsHandlerById, todoHandler, todoHandlerById, updatePost, usersByIdHandler, usersHandler)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import Routes (API, api)
import Servant (Handler, ServerT, hoistServer, serve, type (:<|>) ((:<|>)))

-- | Convert AppM to Handler by supplying the config
runAppM :: AppConfig -> AppM a -> Handler a
runAppM config action = runReaderT action config

-- | Server definition using AppM monad
server :: ServerT API AppM
server = usersRoutes :<|> todoRoutes :<|> commentRoutes :<|> postsRoutes
  where
    usersRoutes = usersHandler :<|> usersByIdHandler
    todoRoutes = todoHandler :<|> todoHandlerById
    commentRoutes = commentHandler :<|> commentsHandlerById
    postsRoutes = postsHandler :<|> postsHandlerById :<|> addPost :<|> updatePost :<|> deletePost

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let config = AppConfig manager "https://jsonplaceholder.typicode.com"
  let hoistedServer = hoistServer api (runAppM config) server
  putStrLn "Running on port 8080..."
  run 8080 (serve api hoistedServer)
