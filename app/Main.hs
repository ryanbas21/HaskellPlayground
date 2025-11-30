{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Handlers (addPost, commentHandler, commentsHandlerById, deletePost, postsHandler, postsHandlerById, todoHandler, todoHandlerById, updatePost, usersByIdHandler, usersHandler)
import Network.Wai.Handler.Warp (run)
import Routes (API, CommentsRoute, PostsRoute, TodoRoute, UserRoute, api)
import Servant (Server, serve, type (:<|>) ((:<|>)))

usersRoutes :: Server UserRoute
usersRoutes = usersHandler :<|> usersByIdHandler

commentRoutes :: Server CommentsRoute
commentRoutes = commentHandler :<|> commentsHandlerById

todoRoutes :: Server TodoRoute
todoRoutes = todoHandler :<|> todoHandlerById

postsRoutes :: Server PostsRoute
postsRoutes = postsHandler :<|> postsHandlerById :<|> addPost :<|> updatePost :<|> deletePost

server :: Server API
server = usersRoutes :<|> todoRoutes :<|> commentRoutes :<|> postsRoutes

main :: IO ()
main = do
  putStrLn "Running on port 8080..."
  run 8080 (serve api server)
