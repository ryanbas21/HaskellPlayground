{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Todo.Posts (Posts (..)) where

import Data.Aeson

data Posts = Posts
  { userId :: Int,
    postId :: Int,
    title :: String,
    body :: String
  }
  deriving (Show, Eq)

instance ToJSON Posts where
  toJSON (Posts userId postId title body) = object ["userId" .= userId, "id" .= postId, "title" .= title, "body" .= body]

instance FromJSON Posts where
  parseJSON = withObject "Posts" $ \v ->
    Posts
      <$> v .: "userId"
      <*> v .: "id"
      <*> v .: "title"
      <*> v .: "body"
