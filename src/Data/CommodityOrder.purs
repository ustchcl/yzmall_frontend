module Yzmalal.Data.CommodityOrder where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Either (Either)
import Yzmall.Data.PreciseDateTime (PreciseDateTime(..), fromString)

data OrderStatus
  = WaitForPayment
  | WaitForPaymentResult
  | WaitForDeliver
  | Delivered
  | WaitForRefund
  | Refunded

type CommodityOrder = 
  { id :: Int
  , accountId :: Int
  , commodityId :: Int
  , tag :: Int
  , amount :: Int
  , commissionCost :: Number
  , rebateCost :: Number
  , payCost :: Number
  , addressId :: Int
  , process :: OrderStatus
  , payPass :: Int
  , payId :: Int
  , expressCompany :: Int
  , expressId :: Int
  , createTime :: PreciseDateTime
  , payTime :: PreciseDateTime
  , deliverTime :: PreciseDateTime
  , refundTime :: PreciseDateTime
  }


status :: String -> OrderStatus
status "WAIT_FOR_PAYMENT" = WaitForPayment
status "WAIT_FOR_PAYMENT_RESULT" = WaitForPaymentResult 
status "WAIT_FOR_DELIVER" = WaitForDeliver
status "DELIVERED" = Delivered
status "WAIT_FOR_REFUND" = WaitForRefund
status _ = Refunded

decodeCommodityOrder :: Json -> Either String CommodityOrder
decodeCommodityOrder json = do
  obj <- decodeJson json
  id <- obj .: "id"
  accountId <- obj .: "accountId"
  commodityId <- obj .: "commodityId"
  tag <- obj .: "tag"
  amount <- obj .: "amount"
  commissionCost <- obj .: "commissionCost"
  rebateCost <- obj .: "rebateCost"
  payCost <- obj .: "payCost"
  addressId <- obj .: "addressId"
  process <- status <$> obj .: "process"
  payPass <- obj .: "payPass"
  payId <- obj .: "payId"
  expressCompany <- obj .: "expressCompany"
  expressId <- obj .: "expressId"
  createTime <- fromString =<< obj .: "createTime"
  payTime <- fromString =<< obj .: "payTime"
  deliverTime <- fromString =<< obj .: "deliverTime"
  refundTime <- fromString =<< obj .: "refundTime"
  pure { id, accountId, commodityId, tag, amount, commissionCost, rebateCost, payCost, addressId, process, payPass, payId, expressCompany, expressId, createTime, payTime, deliverTime, refundTime }