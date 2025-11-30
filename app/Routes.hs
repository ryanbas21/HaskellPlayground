{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Routes (API, api, UserRoute, PostsRoute, CommentsRoute, TodoRoute) where

import Servant
  ( Delete,
    Get,
    JSON,
    NoContent,
    Patch,
    Proxy (..),
    type (:<|>),
    type (:>),
  )
import Servant.API (Capture, Post, ReqBody)
import Todo.Comments (Comment)
import Todo.Posts (Posts)
import Todo.Todos (Todo)
import Todo.Users (User)

type CommentsRoute =
  "comments" :> Get '[JSON] [Comment]
    :<|> "comments"
      :> Capture "x" Int
      :> Get '[JSON] Comment

type TodoRoute =
  "todos"
    :> Get '[JSON] [Todo]
    :<|> "todos"
      :> Capture "x" Int
      :> Get '[JSON] Todo

type UserRoute =
  "users"
    :> Get '[JSON] [User]
    :<|> "users"
      :> Capture "x" Int
      :> Get '[JSON] User

type PostsRoute =
  "posts" :> Get '[JSON] [Posts]
    :<|> "posts"
      :> Capture "x" Int
      :> Get '[JSON] Posts
    :<|> "posts"
      :> ReqBody '[JSON] Posts
      :> Post '[JSON] Posts
    :<|> "posts"
      :> Capture "x" Int
      :> ReqBody '[JSON] Posts
      :> Patch '[JSON] Posts
    :<|> "posts"
      :> Capture "x" Int
      :> Delete '[JSON] NoContent

type API = UserRoute :<|> TodoRoute :<|> CommentsRoute :<|> PostsRoute

api :: Proxy API
api = Proxy
