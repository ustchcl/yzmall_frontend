-- | We can represent valid locations in our application with a simple sum type. This will cause
-- | any invalid routes to fail at compile-time. 
-- |
-- | But since the browser represents locations with strings, we'll also need a way to write our 
-- | `Route` type to a string and parse strings into valid `Route` values. It's tedious and error-
-- | prone to maintain separate printing and parsing functions which can fall out of sync with 
-- | another, and even worse to write them manually. Fortunately, the `routing-duplex` library will
-- | help us write a bi-directional codec which solves both problems.
-- |
-- | For more information about the library and to read the tutorial, see:
-- | https://github.com/natefaubion/purescript-routing-duplex/tree/v0.2.0
module Yzmall.Data.Route where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Routing.Duplex (RouteDuplex', as, boolean, int, string, optional, params, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Slug (Slug, generate)
import Slug as Slug
import Yzmall.Data.Route2 as Route2

-- | We'll represent routes in our application with a simple sum type. As the application grows, 
-- | you might want to swap this out with an extensible sum type with `Variant` and have several 
-- | sub-sections. For our small MVP this type will work just fine and will prevent us from trying 
-- | to send users to non-existent routes.
data HomeType 
  = NormalHome
  | LoginHome
  | RegisterHome
  | BindAlipay

parseType :: String -> Maybe HomeType
parseType "login" = Just LoginHome
parseType "register" = Just RegisterHome
parseType "bindAlipay" = Just BindAlipay
parseType "normal" = Just NormalHome
parseType _ = Nothing

instance showHomeType :: Show HomeType where
  show NormalHome = "normal"
  show LoginHome = "login"
  show RegisterHome = "register"
  show BindAlipay = "bindAlipay"

instance eqHomeType :: Eq HomeType where
  eq NormalHome NormalHome = true
  eq LoginHome LoginHome = true
  eq RegisterHome RegisterHome = true
  eq BindAlipay BindAlipay = true
  eq _ _ = false

instance ordHomeType :: Ord HomeType where
  compare NormalHome LoginHome = GT
  compare LoginHome RegisterHome = GT
  compare NormalHome RegisterHome = GT
  compare _ _ = LT

data Route
  = Home Boolean HomeType
  | RegularCommodity HomeType
  | SpecialCommodity HomeType
  | CommodityInfo Slug
  | Login
  | Register
  | AboutUs
  | ReturnPolicy
  | TradeCenter
  | WDTG_ROUTE -- 我的推广
  | WDDS_ROUTE -- 我的d代售
  | WDFX_ROUTE -- 我的分享
  | PurchaseConfirm Slug
  | SpecialPurchaseConfirm Slug
  | PC_ROUTER (Maybe Route2.Route2)
  | PayOrder { isRegular :: Boolean
              , commodityId :: Int
              , priceDesc :: String
              , commodityName :: String 
              , orderId :: Int
              , cardId :: Int
              }



derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

-- | Next, we'll define a bidirectional codec for our route parsing. Our single codec will handle 
-- | both parsing browser locations and serializing our data type to a browser location. We'll skip 
-- | the boilerplate of separate encoding and decoding functions, and we'll ensure our parsing and 
-- | printing is always in sync.
-- | 
-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": "home" /boolean segment / hometype segment
  , "RegularCommodity": "regular" / hometype segment
  , "SpecialCommodity": "special" / hometype segment
  , "Login": "login" / noArgs
  , "Register": "register" / noArgs
  , "AboutUs": "about-us" / noArgs
  , "ReturnPolicy": "return-policy" / noArgs
  , "CommodityInfo": "commodity" / slug segment
  , "WDTG_ROUTE": "wdtg" / noArgs
  , "WDDS_ROUTE": "wdds" / noArgs
  , "WDFX_ROUTE": "wdfx" / noArgs
  , "TradeCenter": "trade-center" / noArgs
  , "PurchaseConfirm": "order" / slug segment
  , "SpecialPurchaseConfirm": "special-order" / slug segment
  , "PC_ROUTER": "personal-center" / optional Route2.routeCodec
  , "PayOrder": "pay_order" ? { isRegular : boolean , commodityId : int , priceDesc : string , commodityName : string , orderId : int , cardId : int }
  }

-- | This combinator transforms a codec over `String` into one that operatos on the `Slug` type.
slug :: RouteDuplex' String -> RouteDuplex' Slug
slug = as Slug.toString (Slug.parse >>> note "Bad slug")

hometype :: RouteDuplex' String -> RouteDuplex' HomeType
hometype = as show (parseType >>> note "Bad type")

-- category :: RouteDuplex' Boolean -> RouteDuplex' String
-- cate

testSlug :: Slug
testSlug = unsafePartial $ fromJust (generate "sss-sss")