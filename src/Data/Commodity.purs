module Yzmall.Data.Commodity where


import Prelude

import Data.Maybe (Maybe(..))
import Yzmall.Data.Avatar (Avatar)

data CommodityCategory = Regular | Special
derive instance eqCommodityCategory :: Eq CommodityCategory
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

forkData :: Commodity 
forkData = 
  { id : 1
  , name : "高端床品四件套"
  , category : Regular
  , tag : []
  , price : 1000.0
  , gold : 1.0
  , stock : 100
  , sale : 5
  , onSale : true
  , recommend : true
  , thumbnial : Nothing
  , picture : Nothing
  }