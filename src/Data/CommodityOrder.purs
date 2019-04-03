module Yzmalal.Data.CommodityOrder where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Decode.Class (decodeJArray)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Halogen.HTML.Elements.Keyed (table_)
import Type.Data.Boolean (kind Boolean)
import Yzmall.Data.PreciseDateTime (PreciseDateTime(..), fromString)

data OrderStatus
  = WaitForPayment
  | WaitForPaymentResult
  | WaitForDeliver
  | Delivered
  | WaitForRefund
  | Refunded

derive instance eqOrderStatus :: Eq OrderStatus

instance showOrderStatus :: Show OrderStatus where
  show WaitForPayment = "待支付"
  show WaitForPaymentResult = "等待支付结果"
  show WaitForDeliver = "待发货"
  show Delivered = "已发货"
  show WaitForRefund = "等待退款"
  show Refunded = "已退款"

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
  , payPass :: Maybe String 
  , payId :: Maybe String
  , expressCompany :: Maybe String
  , expressId :: Maybe String
  , createTime :: PreciseDateTime
  , payTime :: Maybe PreciseDateTime
  , deliverTime :: Maybe PreciseDateTime
  , refundTime :: Maybe PreciseDateTime
  }


status :: String -> OrderStatus
status "WAIT_FOR_PAYMENT" = WaitForPayment
status "WAIT_FOR_PAYMENT_RESULT" = WaitForPaymentResult 
status "WAIT_FOR_DELIVER" = WaitForDeliver
status "DELIVERED" = Delivered
status "WAIT_FOR_REFUND" = WaitForRefund
status _ = Refunded

decodeArrayCommodityOrder :: Json -> Either String (Array CommodityOrder)
decodeArrayCommodityOrder = sequence <<< (map decodeCommodityOrder) <=< decodeJArray


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
  payTime <- parseTM $ obj .: "payTime"
  deliverTime <- parseTM $ obj .: "deliverTime"
  refundTime <- parseTM $ obj .: "refundTime"
  pure { id, accountId, commodityId, tag, amount, commissionCost, rebateCost, payCost, addressId, process, payPass, payId, expressCompany, expressId, createTime, payTime, deliverTime, refundTime }

parseTM :: Either String String -> Either String (Maybe PreciseDateTime) 
parseTM (Left _) = Right Nothing
parseTM (Right t) = 
  let r = fromString t
  in 
  case r of 
    Left er -> Right Nothing
    Right t ->  Right $ Just t

  
type PayForOrderResult = 
  { expectVcode :: Boolean
  , order :: CommodityOrder
  }

decodePayForOrderResult  :: Json -> Either String PayForOrderResult
decodePayForOrderResult json = do
  obj <- decodeJson json
  expectVcode <- obj .: "expectVcode"
  order <- decodeCommodityOrder =<< obj .: "order"
  pure {expectVcode, order}