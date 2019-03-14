module Yzmall.Page.PurchaseConfirm where

import Halogen.Themes.Bootstrap4
import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Slug (Slug)
import Yzmall.Data.Account (Account)
import Yzmall.Utils (cls, style, (<+>), (->>))

type Input = 
  { slug :: Slug
  }

type State = Int