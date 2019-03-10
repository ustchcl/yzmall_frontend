module Yzmall.Data.Commodity where


import Yzmall.Data.Avatar (Avatar)
import Data.Maybe (Maybe)

data CommodityCategory = Regular | Special
type CommodityTag = String

type CommodityRep row =
  ( id :: Int
  , name :: String
  , category :: CommodityCategory
  , tag :: Array CommodityTag
  , price :: Number
  , gold :: Number
  , stock :: Int
  , sale :: Int
  , onSale :: Boolean
  , recommend :: Boolean
  , thumbnial :: Maybe Avatar
  , picture :: Maybe Avatar
  | row
  )

type Commodity = { | CommodityRep () }

type CommodityId = Int

