{-# LANGUAGE OverloadedStrings #-}

module Utils.Http
  ( fetchAll,
    fetchById,
    create,
    delete,
    deleteablePosts,
    HasPath (..),
    patch,
    patchablePosts,
    Resource (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

data Resource = Users | Todos | Comments | Posts

class HasPath a where
  toPath :: a -> String

instance HasPath Resource where
  toPath Users = "users"
  toPath Todos = "todos"
  toPath Comments = "comments"
  toPath Posts = "posts"

-- Wrapper for patchable resources
newtype Patchable = Patchable Resource

patchablePosts :: Patchable
patchablePosts = Patchable Posts

instance HasPath Patchable where
  toPath (Patchable r) = toPath r

newtype Deleteable = Deleteable Resource

instance HasPath Deleteable where
  toPath (Deleteable r) = toPath r

deleteablePosts :: Deleteable
deleteablePosts = Deleteable Posts

buildUrl :: (HasPath a) => String -> a -> Maybe Int -> String
buildUrl baseUrl resource Nothing = baseUrl ++ "/" ++ toPath resource
buildUrl baseUrl resource (Just rid) = baseUrl ++ "/" ++ toPath resource ++ "/" ++ show rid

fetcher :: (FromJSON a) => Manager -> String -> IO (Either String a)
fetcher manager url = do
  request <- parseRequest url
  response <- httpLbs request manager
  pure $ eitherDecode $ responseBody response

poster :: (FromJSON a, ToJSON b) => Manager -> String -> b -> IO (Either String a)
poster manager url body = do
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "POST",
            requestBody = RequestBodyLBS $ encode body,
            requestHeaders = [("Content-Type", "application/json")]
          }
  response <- httpLbs request manager
  pure $ eitherDecode $ responseBody response

patcher :: (FromJSON a, ToJSON b) => Manager -> String -> b -> IO (Either String a)
patcher manager url body = do
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "PATCH", -- uppercase!
            requestBody = RequestBodyLBS $ encode body,
            requestHeaders = [("Content-Type", "application/json")]
          }
  response <- httpLbs request manager
  pure $ eitherDecode $ responseBody response

deleter :: (FromJSON a) => Manager -> String -> IO (Either String a)
deleter manager url = do
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "DELETE", -- uppercase!
            requestHeaders = [("Content-Type", "application/json")]
          }
  response <- httpLbs request manager
  pure $ eitherDecode $ responseBody response

fetchAll :: (FromJSON a) => Manager -> String -> Resource -> IO (Either String [a])
fetchAll manager baseUrl resource = fetcher manager $ buildUrl baseUrl resource Nothing

fetchById :: (FromJSON a) => Manager -> String -> Resource -> Int -> IO (Either String a)
fetchById manager baseUrl resource rid = fetcher manager $ buildUrl baseUrl resource (Just rid)

create :: (FromJSON a, ToJSON b) => Manager -> String -> Resource -> b -> IO (Either String a)
create manager baseUrl resource = poster manager (buildUrl baseUrl resource Nothing)

patch :: (FromJSON a, ToJSON b) => Manager -> String -> Patchable -> Int -> b -> IO (Either String a)
patch manager baseUrl resource rid = patcher manager (buildUrl baseUrl resource (Just rid))

delete :: (FromJSON a) => Manager -> String -> Deleteable -> Int -> IO (Either String a)
delete manager baseUrl resource rid = deleter manager (buildUrl baseUrl resource (Just rid))
