module Handlers (addPost, commentHandler, commentsHandlerById, deletePost, usersHandler, usersByIdHandler, healthHandler, todoHandler, todoHandlerById, postsHandler, postsHandlerById, updatePost) where

import Config (AppConfig (httpManager, todosBaseUrl), AppM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (asks)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text, pack)
import Servant (Handler, errBody, throwError)
import Servant.Server (err500)
import Todo.Comments (Comment)
import Todo.Posts (Posts)
import Todo.Todos (Todo)
import Todo.Users (User)
import Utils.Http
  ( Resource (Comments, Posts, Todos, Users),
    create,
    delete,
    deleteablePosts,
    fetchAll,
    fetchById,
    patch,
    patchablePosts,
  )

usersHandler :: AppM [User]
usersHandler = do
  manager <- asks httpManager
  baseUrl <- asks todosBaseUrl
  result <- liftIO $ fetchAll manager baseUrl Users
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack "Failed to fetch users"}
    Right users -> pure users

usersByIdHandler :: Int -> AppM User
usersByIdHandler userId = do
  manager <- asks httpManager
  baseUrl <- asks todosBaseUrl
  result <- liftIO $ fetchById manager baseUrl Users userId
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack $ "Failed to fetch user with id " ++ show userId}
    Right v -> pure v

healthHandler :: Handler Text
healthHandler = return $ pack "Healthy!"

todoHandler :: AppM [Todo]
todoHandler = do
  manager <- asks httpManager
  baseUrl <- asks todosBaseUrl
  result <- liftIO $ fetchAll manager baseUrl Todos
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack "Failed to fetch todos"}
    Right v -> pure v

todoHandlerById :: Int -> AppM Todo
todoHandlerById todoId = do
  manager <- asks httpManager
  baseUrl <- asks todosBaseUrl
  result <- liftIO $ fetchById manager baseUrl Todos todoId
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack $ "Failed to fetch todo with id " ++ show todoId}
    Right v -> pure v

commentHandler :: AppM [Comment]
commentHandler = do
  manager <- asks httpManager
  baseUrl <- asks todosBaseUrl
  result <- liftIO $ fetchAll manager baseUrl Comments
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack "Failed to fetch comments"}
    Right v -> pure v

commentsHandlerById :: Int -> AppM Comment
commentsHandlerById commentid = do
  manager <- asks httpManager
  baseUrl <- asks todosBaseUrl
  result <- liftIO $ fetchById manager baseUrl Comments commentid
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack $ "Failed to fetch comments with id " ++ show commentid}
    Right v -> pure v

postsHandler :: AppM [Posts]
postsHandler = do
  manager <- asks httpManager
  baseUrl <- asks todosBaseUrl
  result <- liftIO $ fetchAll manager baseUrl Posts
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack "Failed to fetch posts"}
    Right v -> pure v

postsHandlerById :: Int -> AppM Posts
postsHandlerById postId = do
  manager <- asks httpManager
  baseUrl <- asks todosBaseUrl
  result <- liftIO $ fetchById manager baseUrl Posts postId
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack $ "Failed to fetch post with id " ++ show postId}
    Right v -> pure v

addPost :: Posts -> AppM Posts
addPost post = do
  manager <- asks httpManager
  baseUrl <- asks todosBaseUrl
  response <- liftIO $ create manager baseUrl Posts post
  case response of
    Left _ -> throwError $ err500 {errBody = LBS.pack "failed to add post"}
    Right v -> pure v

updatePost :: Int -> Posts -> AppM Posts
updatePost postid post = do
  manager <- asks httpManager
  baseUrl <- asks todosBaseUrl
  response <- liftIO $ patch manager baseUrl patchablePosts postid post
  case response of
    Left _ -> throwError $ err500 {errBody = LBS.pack "failed to update the post"}
    Right v -> pure v

deletePost :: Int -> Posts -> AppM Posts
deletePost postid post = do
  manager <- asks httpManager
  baseUrl <- asks todosBaseUrl
  response <- liftIO $ delete manager baseUrl deleteablePosts postid post
  case response of
    Left _ -> throwError $ err500 {errBody = LBS.pack "failed to delete the post"}
    Right v -> pure v
