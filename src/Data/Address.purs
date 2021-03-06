module Yzmall.Data.Address where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Decode.Class (decodeJArray)
import Data.Either (Either)
import Data.Traversable (sequence)

type Address = 
  { id :: Int
  , accountId :: Int
  , name :: String
  , address :: String
  , phone :: String
  }

decodeAddress :: Json -> Either String Address
decodeAddress json = do
  obj <- decodeJson json
  id <- obj .: "id"
  accountId <- obj .: "accountId"
  name <- obj .: "name"
  address <- obj .: "address"
  phone <- obj .: "phone"
  pure { id, accountId, name, address, phone }

decodeArrayAddress :: Json -> Either String (Array Address)
decodeArrayAddress = sequence <<< (map decodeAddress) <=< decodeJArray