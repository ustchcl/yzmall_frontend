-- | Conduit uses a REST API for resource management. This module defines endpoints in a data type
-- | which ensures invalid endpoints fail to compile. Since the library we use to perform requests
-- | uses string URLs, we need to be able to write our endpoints to string values. We'll use the
-- | `routing-duplex` library to get this string conversion for free.
-- |
-- | In a larger application we might code-generate this module from an Open API or Swagger 
-- | spec, or split it into several separate modules.
-- |
-- | This module takes the same approach as the `Conduit.Data.Route` module. For a more in-depth  
-- | treatment of representing endpoints and routes as a data type that can be parsed from and  
-- | printed to `String` values, I recommend reading the documentation in that module.
module Yzmall.Api.Endpoint where

import Prelude hiding ((/))

import Affjax.RequestBody (RequestBody(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Routing.Duplex (RouteDuplex', int, optional, prefix, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Slug (Slug)
import Yzmall.Data.Commodity (CommodityCategory)
import Yzmall.Data.Route (slug)

-- | First, let's define a few types necessary for our larger `Endpoint` type.

-- | Some endpoints are paginated and accept a limit (maximum count) and offset (number of items
-- | to skip over). Since some endpoints accept pagination in addition to other parameters, we'll
-- | create a row that can be shared by multiple types.
type PaginationRep =
  ( page :: Maybe Int 
  , size :: Maybe Int 
  )

-- | This record type is useful for endpoints that only need pagination information.
type Pagination = { | PaginationRep  }


-- | This data type captures each endpoint our API supports. In a larger application this would be
-- | tedious to maintain, and it's more common to generate endpoints from a Swagger or Open API
-- | spec. For the time being, though, we'll take the same approach as we did for our routes and
-- | create an encompassing sum type to represent all endpoints. With this type, requests to 
-- | invalid endpoints (endpoints not captured in this type) will fail to compile.
data Endpoint
  = 
  -- 账号服务
  
  -- Address 
  MyAddresses
  | AddAddress
  | DeleteAddress Slug
  | GetAddress Slug

  | Invitees
  | Logout
  | BindAlipay
  -- BankCard
  | MyBankCards
  | GetBankCard Slug
  | AddBankCard
  | DeleteBankCard Slug
  -- Order
  | MyAccountInfo
  | VerifyIDCard
  -- 公共账号服务
  | Login
  | CreateVcode
  | ResetPassword
  | CreateAccount
  | SetName
  | ViewCommodity CommodityParams
  | GetCommodity Slug
-- 商品服务
  | ViewOrder
  | PayForOrder Slug
  | CreateOrderSpecial Slug
  | PayForOrderSpecial Slug
  | DeleteOrder Slug
  | CreateACTSellCommission
  | CreateACTSellRebate
  | CreateACTSellRush
  | CreateOrder Slug
  | ACTSharedRecord
  | MyACTSells
  | ViewCommissions


type CommodityParams = 
  { category :: String
  | PaginationRep
  }

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

-- Our codec will cause a compile-time error if we fail to handle any of our 
-- route cases.

-- | We need to be able to write our `Endpoint` type to a valid path in order to make requests. We
-- | can use `routing-duplex` the same way we did with our `Route` type to provide both a printer 
-- | (write to `String`) and a parser (parse from `String`) that stays in sync with our `Endpoint`
-- | type automatically.
-- | 
-- | For a full treatment of how this function produces both a parser and printer guaranteed to
-- | produce valid paths, see the `routing-duplex` tutorial:
-- | https://github.com/natefaubion/purescript-routing-duplex/tree/v0.2.0
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "MyAddresses": "account" / "address" / "" / noArgs 
  , "AddAddress": "account" / "address" / noArgs
  , "DeleteAddress": "account" / "address" / slug segment / "delete"
  , "GetAddress": "account" / "address" / slug segment 
  , "MyAccountInfo": "account" / "mine" / noArgs
  , "VerifyIDCard": "account" / "mine" / "setName" / noArgs
  , "Login": "public" / "account" / "login" / noArgs
  , "CreateVcode": "public" / "account" / "createVcode" / noArgs
  , "ResetPassword": "public" / "account" / "resetPassword" / noArgs
  , "CreateAccount": "public" / "account" / "createAccount" / noArgs
  , "SetName": "account" / "mine" / "setName" / noArgs
  , "Invitees": "account" / "invitees" / noArgs
  , "Logout" : "account" / "logout" / noArgs
  , "BindAlipay": "account" / "mine" / "setAlipay" / noArgs
  , "ViewCommodity": "public" / "commodity" / "" ? 
        { category: string
        , page: optional <<< int 
        , size: optional <<< int 
        }
  , "GetCommodity": "public" / "commodity" / slug segment
  -- 商品服务
  , "ViewOrder" : "commodity" / "order" / "" / noArgs
  , "PayForOrder" : "commodity" / "order" / slug segment / "pay"
  , "CreateOrderSpecial": "commodity" / slug segment / "createOrderSpecial"
  , "PayForOrderSpecial": "commodity" / "order" / slug segment / "paySpecial"
  , "DeleteOrder": "commodity" / "order" / slug segment / "delete"
  , "CreateACTSellCommission": "commodity" / "createMYTSellCommission" / noArgs
  , "CreateACTSellRebate": "commodity" / "createMYTSellRebate" / noArgs
  , "CreateACTSellRush": "commodity" / "createMYTSellRush" / noArgs
  , "CreateOrder": "commodity" / slug segment / "createOrder"
  , "ACTSharedRecord": "commodity" / "mytSharedRecord" / noArgs
  , "MyACTSells": "commodity" / "mytSell" / "" / noArgs
  , "ViewCommissions": "commodity" / "commission" / "" / noArgs
  -- BankCard
  , "MyBankCards": "account" / "bankCard" / "" / noArgs 
  , "AddBankCard": "account" / "bankCard" / noArgs
  , "DeleteBankCard": "account" / "bankCard" / slug segment / "delete"
  , "GetBankCard": "account" / "bankCard" / slug segment 
  } 
