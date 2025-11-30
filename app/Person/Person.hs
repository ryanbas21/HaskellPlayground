{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Person.Person (Person (..)) where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Lib ()

data Person
  = Person {name :: Text, age :: Int}
  deriving (Generic, Show)

instance ToJSON Person where
  toJSON (Person name age) = object ["name" .= name, "age" .= age]

instance FromJSON Person where
  parseJSON = withObject "Person" $ \v -> Person <$> v .: "name" <*> v .: "age"
