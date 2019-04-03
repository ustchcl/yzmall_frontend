module Yzmall.Data.Commodity where


import Prelude

import Affjax.RequestBody (RequestBody(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Decode.Class (decodeJArray)
import Data.Either (Either, hush, isRight)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set.NonEmpty (filter)
import Data.Traversable (sequence)
import Web.HTML.HTMLTrackElement (setDefault)
import Yzmall.Data.Avatar (Avatar, parse)

data CommodityCategory = Regular | Special
derive instance eqCommodityCategory :: Eq CommodityCategory
instance showCommodityCategory :: Show CommodityCategory where
  show Regular = "REGULAR"
  show Special = "SPECIAL"

type CommodityTag = 
  { id :: Int 
  , content :: String
  }

type CommodityRep row =
  ( id :: Int
  , name :: String
  , category :: CommodityCategory
  , tag :: Array CommodityTag
  , price :: Number
  , gold :: Number
  , stock ::Int
  , sale :: Int
  , onSale :: Boolean
  , recommend :: Boolean
  , thumbnail :: String
  , picture :: String
  , rebateMYT :: Number
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
  , thumbnail : ""
  , picture : ""
  , rebateMYT: 1.0
  }


decodeArrayCommodity :: Json -> Either String (Array Commodity)
decodeArrayCommodity = sequence <<< (map decodeCommodity) <=< decodeJArray

decodeCommodity :: Json -> Either String Commodity
decodeCommodity json = do
  obj <- decodeJson json
  id <- obj .: "id"
  name <- obj .: "name"
  category <- genCategory <$> obj .: "category"
  price <- obj .: "price"
  -- primeCost <- obj .: "primeCost"
  rebateMYT <- obj .: "rebateMYT"
  gold <- obj .: "gold"
  stock <- obj .: "stock"
  sale <- obj .: "sale"
  onSale <- obj .: "onSale"
  recommend <- obj .: "recommend"
  thumbnail <- obj .: "thumbnail"
  picture <- obj .: "picture"
  tag <- decodeCommodityTags =<< obj .: "tag"
  pure { id, name, category, price, rebateMYT, gold, stock, sale, onSale, recommend, thumbnail, picture, tag }


genCategory :: String -> CommodityCategory
genCategory "SPECIAL" = Special
genCategory _ = Regular

decodeCommodityTags :: Json -> Either String (Array CommodityTag)
decodeCommodityTags = sequence <<< (map decodeCommodityTag) <=< decodeJArray

decodeCommodityTag :: Json -> Either String CommodityTag
decodeCommodityTag json = do
  obj <- decodeJson json
  id <- obj .: "id"
  content <- obj .: "content"
  pure { id, content }

