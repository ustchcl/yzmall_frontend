module Yzmall.Data.BankCard where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Decode.Class (decodeJArray)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Traversable (sequence)

type BankCard = 
  { id :: Int
  , accountId :: Int
  , name :: String
  , cardId :: String
  , region :: String
  , phone :: String
  , bank :: String
  , huijuSignId :: Maybe String
  }

decodeBankCard :: Json -> Either String BankCard
decodeBankCard json = do
  obj <- decodeJson json
  id <- obj .: "id"
  accountId <- obj .: "accountId"
  name <- obj .: "name"
  cardId <- obj .: "cardId"
  region <- obj .: "region"
  phone <- obj .: "phone"
  bank <- obj .: "bank"
  huijuSignId <- obj .: "huijuSignId"
  pure { id, accountId, name, cardId, region, phone, bank, huijuSignId }

decodeArrayBankCard :: Json -> Either String (Array BankCard)
decodeArrayBankCard = sequence <<< (map decodeBankCard) <=< decodeJArray