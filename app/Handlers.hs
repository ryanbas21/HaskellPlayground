module Handlers (addPost, commentHandler, commentsHandlerById, deletePost, usersHandler, usersByIdHandler, healthHandler, todoHandler, todoHandlerById, postsHandler, postsHandlerById, updatePost) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text, pack)
import Servant (Handler, errBody, throwError)
import Servant.Server (err500)
import Todo.Comments (Comment)
import Todo.Posts (Posts)
import Todo.Todos (Todo)
import Todo.Users (User)
import Utils.Http

usersHandler :: Handler [User]
usersHandler = do
  result <- liftIO $ fetchAll Users
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack "Failed to fetch users"}
    Right users -> pure users

usersByIdHandler :: Int -> Handler User
usersByIdHandler userId = do
  result <- liftIO $ fetchById Users userId
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack $ "Failed to fetch user with id " ++ show userId}
    Right v -> pure v

healthHandler :: Handler Text
healthHandler = return $ pack "Healthy!"

todoHandler :: Handler [Todo]
todoHandler = do
  result <- liftIO $ fetchAll Todos
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack "Failed to fetch todos"}
    Right v -> pure v

todoHandlerById :: Int -> Handler Todo
todoHandlerById todoId = do
  result <- liftIO $ fetchById Todos todoId
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack $ "Failed to fetch todo with id " ++ show todoId}
    Right v -> pure v

commentHandler :: Handler [Comment]
commentHandler = do
  result <- (liftIO . fetchAll) Comments
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack "Failed to fetch comments"}
    Right v -> pure v

commentsHandlerById :: Int -> Handler Comment
commentsHandlerById commentid = do
  result <- (liftIO . fetchById Comments) commentid
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack $ "Failed to fetch comments with id " ++ show commentid}
    Right v -> pure v

postsHandler :: Handler [Posts]
postsHandler = do
  result <- (liftIO . fetchAll) Posts
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack "Failed to fetch comments"}
    Right v -> pure v

postsHandlerById :: Int -> Handler Posts
postsHandlerById postId = do
  result <- (liftIO . fetchById Posts) postId
  case result of
    Left _ -> throwError $ err500 {errBody = LBS.pack $ "Failed to fetch post with id " ++ show postId}
    Right v -> pure v

addPost :: Posts -> Handler Posts
addPost post = do
  response <- (liftIO . create Posts) post
  case response of
    Left _ -> throwError $ err500 {errBody = LBS.pack "failed to add post"}
    Right v -> pure v

updatePost :: Int -> Posts -> Handler Posts
updatePost postid post = do
  response <- liftIO $ patch patchablePosts postid post
  case response of
    Left _ -> throwError $ err500 {errBody = LBS.pack "failed to update the post"}
    Right v -> pure v

deletePost :: Int -> Posts -> Handler Posts
deletePost postid post = do
  response <- liftIO $ delete deleteablePosts postid post
  case response of
    Left _ -> throwError $ err500 {errBody = LBS.pack "failed to update the post"}
    Right v -> pure v
