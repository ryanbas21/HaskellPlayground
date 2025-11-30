{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Todo.Todos (Todo) where

import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), object, withObject)
import Data.Aeson.Types ((.:))
import Prelude hiding (id)

data Todo = Todo
  { userId :: Int,
    id :: Int,
    title :: String,
    completed :: Bool
  }
  deriving (Show, Eq)

instance ToJSON Todo where
  toJSON (Todo userId id title completed) = object ["userId" .= userId, "id" .= id, "title" .= title, "completed" .= completed]

instance FromJSON Todo where
  parseJSON = withObject "Todo" $ \v ->
    Todo
      <$> v .: "userId"
      <*> v .: "id"
      <*> v .: "title"
      <*> v .: "completed"
