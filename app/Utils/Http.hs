{-# LANGUAGE OverloadedStrings #-}

module Utils.Http
  ( fetchAll,
    fetchById,
    create,
    delete,
    deleteablePosts,
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

baseUrl :: String
baseUrl = "https://jsonplaceholder.typicode.com"

buildUrl :: (HasPath a) => a -> Maybe Int -> String
buildUrl resource Nothing = baseUrl ++ "/" ++ toPath resource
buildUrl resource (Just rid) = baseUrl ++ "/" ++ toPath resource ++ "/" ++ show rid

fetcher :: (FromJSON a) => String -> IO (Either String a)
fetcher url = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  pure $ eitherDecode $ responseBody response

poster :: (FromJSON a, ToJSON b) => String -> b -> IO (Either String a)
poster url body = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "POST",
            requestBody = RequestBodyLBS $ encode body,
            requestHeaders = [("Content-Type", "application/json")]
          }
  response <- httpLbs request manager
  pure $ eitherDecode $ responseBody response

patcher :: (FromJSON a, ToJSON b) => String -> b -> IO (Either String a)
patcher url body = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "PATCH", -- uppercase!
            requestBody = RequestBodyLBS $ encode body,
            requestHeaders = [("Content-Type", "application/json")]
          }
  response <- httpLbs request manager
  pure $ eitherDecode $ responseBody response

deleter :: (FromJSON a, ToJSON b) => String -> b -> IO (Either String a)
deleter url body = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "DELETE", -- uppercase!
            requestBody = RequestBodyLBS $ encode body,
            requestHeaders = [("Content-Type", "application/json")]
          }
  response <- httpLbs request manager
  pure $ eitherDecode $ responseBody response

fetchAll :: (FromJSON a) => Resource -> IO (Either String [a])
fetchAll resource = fetcher $ buildUrl resource Nothing

fetchById :: (FromJSON a) => Resource -> Int -> IO (Either String a)
fetchById resource rid = fetcher $ buildUrl resource (Just rid)

create :: (FromJSON a, ToJSON b) => Resource -> b -> IO (Either String a)
create resource = poster (buildUrl resource Nothing)

patch :: (FromJSON a, ToJSON b) => Patchable -> Int -> b -> IO (Either String a)
patch resource rid = patcher (buildUrl resource (Just rid))

delete :: (FromJSON a, ToJSON b) => Deleteable -> Int -> b -> IO (Either String a)
delete resource rid = deleter (buildUrl resource (Just rid))
