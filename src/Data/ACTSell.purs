module Yzmall.Data.ACTSell where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Decode.Class (decodeJArray)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Yzmall.Data.PreciseDateTime (PreciseDateTime, fromString)

data  ACTSellCategory 
  = COMMISSION 
  | REBATE

data ACTSellProcess
  = WAIT_FOR_REVIEW
  | WAIT_FOR_PAYMENT_RESULT 
  | SOLD
  | REJECT

instance showACTSellProcess :: Show ACTSellProcess where
    show WAIT_FOR_REVIEW = "待审核"
    show WAIT_FOR_PAYMENT_RESULT = "等待支付结果"
    show SOLD = "已售出"
    show REJECT = "已拒绝"


type ACTSell = 
  { id :: Int
  , accountId :: Int
  , category :: ACTSellCategory
  , price :: Number
  , amount :: Int
  , process :: ACTSellProcess
  , payPass :: Maybe Number
  , payId :: Maybe String
  , createTime :: PreciseDateTime
  , soldTime :: Maybe PreciseDateTime
  , rejectTime :: Maybe PreciseDateTime
  }

decodeArrayACTSell :: Json -> Either String (Array ACTSell)
decodeArrayACTSell = sequence <<< (map decodeACTSell) <=< decodeJArray

decodeACTSell :: Json -> Either String ACTSell
decodeACTSell json = do
  obj <- decodeJson json
  id <- obj .: "id"
  accountId <- obj .: "accountId"
  category <- category_ <$> obj .: "category"
  price <- obj .: "price"
  amount <- obj .: "amount"
  process <- process_ <$> obj .: "process"
  payPass <- obj .: "payPass"
  payId <- obj .: "payId"
  createTime <- fromString =<< obj .: "createTime"
  soldTime <- parseTM $ obj .: "soldTime"
  rejectTime <- parseTM $ obj .: "rejectTime"
  pure { id, accountId, category, price, amount, process, payPass, payId, createTime, soldTime, rejectTime }

category_ :: String -> ACTSellCategory
category_ "COMMISSION" = COMMISSION
category_ _ = REBATE

process_ :: String -> ACTSellProcess
process_ "WAIT_FOR_REVIEW" = WAIT_FOR_REVIEW
process_ "WAIT_FOR_PAYMENT_RESULT" = WAIT_FOR_PAYMENT_RESULT
process_ "SOLD" = SOLD
process_ _ = REJECT


decodePrice :: Json -> Either String Number
decodePrice = decodeJson


type MYTSharedRecord =
  { price :: Number
  , remainPurchase :: Number
  , currentPeriod :: Int
  }

decodeMytSharedRecord :: Json -> Either String MYTSharedRecord
decodeMytSharedRecord json = do
  obj <- decodeJson json
  price <- obj .: "price"
  remainPurchase <- obj .: "remainPurchase"
  currentPeriod <- obj .: "currentPeriod"
  pure {remainPurchase, price, currentPeriod}


parseTM :: Either String String -> Either String (Maybe PreciseDateTime) 
parseTM (Left _) = Right Nothing
parseTM (Right t) = 
  let r = fromString t
  in 
  case r of 
    Left er -> Right Nothing
    Right t ->  Right $ Just t