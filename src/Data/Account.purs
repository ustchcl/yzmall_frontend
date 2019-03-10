module Yzmall.Data.Account where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Either (Either)
import Data.Maybe (Maybe)
import Yzmall.Data.Phone (Phone(..))


data Role = ADMIN | CUSTOMER | SUPPLIER

type Account = 
  { id :: Int
  , inviter :: Maybe Int
  , nickname :: Maybe String
  , phone :: Phone
  , role :: Role
  , grade :: Int
  , name :: Maybe String
  , idCard :: Maybe String
  , gold :: Int
  , regularCommodityCost :: Number
  , specialCommodityCost :: Number
  , commissionBalance :: Number
  , commissionSellFacility :: Number
  , commissionSell :: Number
  , rebateBalance :: Number
  , rebateSell :: Number
  , defaultAddress :: Maybe Int
  , defaultBankCard :: Maybe Int
  }

getNickname :: Account -> Maybe String
getNickname account = account.nickname


decodeAccount :: Json -> Either String Account
decodeAccount json = do
  obj <- decodeJson json
  id <- obj .: "id"
  inviter <- obj .: "inviter"
  nickname <- obj .: "nickname"
  phone <- Phone <$> obj .: "phone"
  role <- mkRole <$> obj .: "role"
  grade <- obj .: "grade"
  name <- obj .: "name"
  idCard <- obj .: "idCard"
  gold <- obj .: "gold"
  regularCommodityCost <- obj .: "regularCommodityCost"
  specialCommodityCost <- obj .: "specialCommodityCost"
  commissionBalance <- obj .: "commissionBalance"
  commissionSellFacility <- obj .: "commissionSellFacility"
  commissionSell <- obj .: "commissionSell"
  rebateBalance <- obj .: "rebateBalance"
  rebateSell <- obj .: "rebateSell"
  defaultAddress <- obj .: "defaultAddress"
  defaultBankCard <- obj .: "defaultBankCard"
  pure { id, inviter, nickname, phone, role, grade, name, idCard, gold, regularCommodityCost, specialCommodityCost, commissionBalance, commissionSellFacility, commissionSell, rebateBalance, rebateSell, defaultAddress, defaultBankCard }

mkRole :: String -> Role
mkRole "ADMIN" = ADMIN
mkRole "SUPPLIER" = SUPPLIER
mkRole _ = CUSTOMER


