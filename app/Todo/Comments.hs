{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Todo.Comments (Comment) where

import Data.Aeson (FromJSON (parseJSON), ToJSON, object, withObject, (.:), (.=))
import Data.Aeson.Types (ToJSON (..))
import Prelude hiding (id)

data Comment = Comment
  { postId :: Int,
    id :: Int,
    comment :: String
  }
  deriving (Show, Eq)

instance ToJSON Comment where
  toJSON (Comment postId id comment) = object ["postId" .= postId, "id" .= id, "name" .= comment]

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \v ->
    Comment
      <$> v .: "postId"
      <*> v .: "id"
      <*> v .: "name"
