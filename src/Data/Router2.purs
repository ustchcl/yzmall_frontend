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
module Yzmall.Data.Route2 where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', as, boolean, params, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

-- | We'll represent routes in our application with a simple sum type. As the application grows, 
-- | you might want to swap this out with an extensible sum type with `Variant` and have several 
-- | sub-sections. For our small MVP this type will work just fine and will prevent us from trying 
-- | to send users to non-existent routes.

data Route2
  =
   PersonalCenter
  | AddressEditor
  | MyOrders
  | BankCardEditor
  | AccountInfo
  | AddressSelector
  | BankCardSelector


derive instance genericRoute :: Generic Route2 _
derive instance eqRoute :: Eq Route2
derive instance ordRoute :: Ord Route2

instance showRoute :: Show Route2 where
  show = genericShow

-- | Next, we'll define a bidirectional codec for our route parsing. Our single codec will handle 
-- | both parsing browser locations and serializing our data type to a browser location. We'll skip 
-- | the boilerplate of separate encoding and decoding functions, and we'll ensure our parsing and 
-- | printing is always in sync.
-- | 
-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
routeCodec :: RouteDuplex' Route2
routeCodec = root $ sum
  { "AccountInfo":  "account-info" / noArgs
  , "PersonalCenter":  "home" /  noArgs
  , "AddressEditor":  "address-editor" / noArgs
  , "MyOrders":  "my-orders" / noArgs
  , "BankCardEditor":  "bankcard-editor" / noArgs
  , "AddressSelector":  "address-selecttor" / noArgs
  , "BankCardSelector": "bankcard-selector" / noArgs
  }
