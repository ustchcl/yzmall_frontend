module Yzmall.Page.PurchaseConfirm where

import Halogen.Themes.Bootstrap4
import Prelude

import Affjax.RequestBody (RequestBody(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData)
import Slug (Slug)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Commodity (Commodity)
import Yzmall.Utils (cls, style, (<+>), (->>))

type Input = 
  { slug :: Slug
  }

type State = 
  { account :: Maybe Account
  , address :: RemoteData String Address 
  , commodity :: RemoteData String Commodity
  }