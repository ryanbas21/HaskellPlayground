{-# LANGUAGE OverloadedStrings #-}

module Todo.Users (Address, Company, Geo, User) where

import Data.Aeson

{-{
    "id": 1,
    "name": "Leanne Graham",
    "username": "Bret",
    "email": "Sincere@april.biz",
    "address": {
      "street": "Kulas Light",
      "suite": "Apt. 556",
      "city": "Gwenborough",
      "zipcode": "92998-3874",
      "geo": {
        "lat": "-37.3159",
        "lng": "81.1496"
      }
    },
    "phone": "1-770-736-8031 x56442",
    "website": "hildegard.org",
    "company": {
      "name": "Romaguera-Crona",
      "catchPhrase": "Multi-layered client-server neural-net",
      "bs": "harness real-time e-markets"
    }
  }
  -}

data Geo = Geo
  { lat :: String,
    lng :: String
  }
  deriving (Show)

{- Geo
>>> decode "{\"lat\":\"100\",\"lng\":\"100\"}" :: Maybe Geo
Just (Geo {lat = "100", lng = "100"})
-}
instance FromJSON Geo where
  parseJSON = withObject "Geo" $ \s ->
    Geo
      <$> s .: "lat"
      <*> s .: "lng"

{- Geo
>>> encode Geo { lat = "100", lng = "100" }
"{\"lat\":\"100\",\"lng\":\"100\"}"
-}
instance ToJSON Geo where
  toJSON (Geo gLat gLong) = object ["lat" .= gLat, "lng" .= gLong]

data Address = Address
  { street :: String,
    suite :: String,
    city :: String,
    zipcode :: String,
    geo :: Geo
  }
  deriving (Show)

{-Address
 -
>>> decode "{\"city\":\"Armonk\",\"geo\":{\"lat\":\"123\",\"lng\":\"123\"},\"street\":\"123 street\",\"suite\":\"1\",\"zipcode\":\"10504\"}" :: Maybe Address
Just (Address {street = "123 street", suite = "1", city = "Armonk", zipcode = "10504", geo = Geo {lat = "123", lng = "123"}})

-}
instance FromJSON Address where
  parseJSON = withObject "Address" $ \v ->
    Address
      <$> v .: "street"
      <*> v .: "suite"
      <*> v .: "city"
      <*> v .: "zipcode"
      <*> v .: "geo"

{-
>>> encode Address { street = "123 street", suite = "1", city = "Armonk", zipcode = "10504", geo = Geo { lat = "123", lng = "123" } }
"{\"city\":\"Armonk\",\"geo\":{\"lat\":\"123\",\"lng\":\"123\"},\"street\":\"123 street\",\"suite\":\"1\",\"zipcode\":\"10504\"}"
-}
instance ToJSON Address where
  toJSON (Address astreet asuite acity azipcode ageo) = object ["street" .= astreet, "suite" .= asuite, "city" .= acity, "zipcode" .= azipcode, "geo" .= ageo]

data Company = Company
  { companyName :: String,
    catchPhrase :: String,
    bs :: String
  }
  deriving (Show)

{-
>>> encode Company { companyName = "google", catchPhrase = "a company", bs = "aaa"}
"{\"bs\":\"aaa\",\"catchPhrase\":\"a company\",\"companyName\":\"google\"}"
 -}
instance ToJSON Company where
  toJSON (Company acompanyName acatchPhrase aBS) = object ["companyName" .= acompanyName, "catchPhrase" .= acatchPhrase, "bs" .= aBS]

{-
>>> decode "{\"bs\":\"aaa\",\"catchPhrase\":\"a company\",\"name\":\"google\"}" :: Maybe Company
Just (Company {companyName = "google", catchPhrase = "a company", bs = "aaa"})

 -}
instance FromJSON Company where
  parseJSON = withObject "Company" $ \v ->
    Company
      <$> v .: "name"
      <*> v .: "catchPhrase"
      <*> v .: "bs"

data User = User
  { userId :: Int,
    name :: String,
    username :: String,
    email :: String,
    address :: Address,
    phone :: String,
    website :: String,
    company :: Company
  }
  deriving (Show)

{-
>>> decode "{\"address\":{\"city\":\"Armonk\",\"geo\":{\"lat\":\"123\",\"lng\":\"123\"},\"street\":\"123, street\",\"suite\":\"1\",\"zipcode\":\"10504\"},\"company\":{\"bs\":\"bs\",\"catchPhrase\":\"123 google\",\"companyName\":\"Google\"},\"email\":\"myemail@email.com\",\"id\":888,\"name\":\"my name\",\"phone\":\"123453242\",\"username\":\"myusername\",\"website\":\"google.com\"}" :: Maybe User
Nothing
-}
instance FromJSON User where
  parseJSON = withObject "User" $ \v ->
    User
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "username"
      <*> v .: "email"
      <*> v .: "address"
      <*> v .: "phone"
      <*> v .: "website"
      <*> v .: "company"

{-
>>> encode User { userId = 888, name = "my name", username = "myusername", email = "myemail@email.com", address = Address { street = "123, street", suite = "1", city = "Armonk", zipcode = "10504", geo = Geo { lat = "123", lng = "123" } }, phone = "123453242", website = "google.com", company = Company { companyName = "Google", catchPhrase = "123 google", bs = "bs" } }
"{\"address\":{\"city\":\"Armonk\",\"geo\":{\"lat\":\"123\",\"lng\":\"123\"},\"street\":\"123, street\",\"suite\":\"1\",\"zipcode\":\"10504\"},\"company\":{\"bs\":\"bs\",\"catchPhrase\":\"123 google\",\"companyName\":\"Google\"},\"email\":\"myemail@email.com\",\"id\":888,\"name\":\"my name\",\"phone\":\"123453242\",\"username\":\"myusername\",\"website\":\"google.com\"}"
-}
instance ToJSON User where
  toJSON (User _id _name _username _email _address _phone _website _company) = object ["id" .= _id, "name" .= _name, "username" .= _username, "email" .= _email, "address" .= _address, "phone" .= _phone, "website" .= _website, "company" .= _company]
