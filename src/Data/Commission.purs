module Yzmall.Data.Commission where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Decode.Class (decodeJArray)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Yzmall.Data.PreciseDateTime (PreciseDateTime(..), fromString)


type Commission = 
  { id :: Int
  , accountId :: Int
  , commodityId :: Int
  , tag :: Maybe Int
  , amount :: Int
  , process :: String
  , expressCompany :: Maybe String
  , expressId :: Maybe String
  , createTime :: PreciseDateTime
  , soldTime :: Maybe PreciseDateTime
  , deliverTime :: Maybe PreciseDateTime
  }

decodeCommission :: Json -> Either String Commission
decodeCommission json = do
  obj <- decodeJson json
  id <- obj .: "id"
  accountId <- obj .: "accountId"
  commodityId <- obj .: "commodityId"
  tag <- obj .: "tag"
  amount <- obj .: "amount"
  process <- obj .: "process"
  expressCompany <- obj .: "expressCompany"
  expressId <- obj .: "expressId"
  createTime <- fromString =<< obj .: "createTime"
  soldTime <- parseTM $ obj .: "soldTime"
  deliverTime <-  parseTM $ obj .: "deliverTime"

  pure { id, accountId, commodityId, tag, amount,  process, expressCompany, expressId, createTime, soldTime, deliverTime}

decodeArrayCommission :: Json -> Either String (Array Commission)
decodeArrayCommission = sequence <<< (map decodeCommission) <=< decodeJArray


parseTM :: Either String String -> Either String (Maybe PreciseDateTime) 
parseTM (Left _) = Right Nothing
parseTM (Right t) = 
  let r = fromString t
  in 
  case r of 
    Left er -> Right Nothing
    Right t ->  Right $ Just t