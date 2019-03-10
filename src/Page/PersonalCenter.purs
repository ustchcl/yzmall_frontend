module Yzmall.Page.PersonalCenter where

import Halogen.Themes.Bootstrap4
import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Yzmall.Data.Account (Account)
import Yzmall.Utils (cls, style, (<+>), (->>))


type State = Maybe Account

data Query a
  = Initialize a

component :: forall m. Maybe Account -> H.Component HH.HTML Query Unit Void m
component account =
    H.component
        { initialState: const account
        , render
        , eval 
        , receiver: const Nothing
        }
    where
      eval :: Query ~> H.ComponentDSL State Query Void m
      eval = case _ of
        Initialize a -> do
          pure a

      render :: State -> H.ComponentHTML Query
      render _ = 
        HH.div
        [ cls listGroup]
        [ renderItem
        , renderItem
        , renderItem
        , renderItem
        , renderItem2

        ]
      

renderItem :: forall p i. H.HTML p i
renderItem =
  HH.a
  [ cls $ listGroupItem <> listGroupItemAction <> listGroupItemDark <> dFlex <> alignItemsCenter <> px2 <> border0 <> borderBottom <> mb3 <> rounded0
  , HP.href "#"
  ]
  [ HH.text "some thing"
  , HH.i
    [ cls $ mlAuto <+> "fas fa-chevron-right" ]
    []
  ]


renderItem2 :: forall p i. H.HTML p i
renderItem2 = 
  HH.a
  [ cls $ listGroupItem <> listGroupItemAction <> listGroupItemDark <> dFlex <> alignItemsCenter <> px2 <> border0 <> borderBottom <> mb3 <> rounded0
  , HP.href "#"
  ]
  [ HH.div_
    [ HH.i
      [ "class" ->> "fas fa-yen-sign" ]
      []
    , HH.text "some thing"
    ]
  , HH.i
    [ cls $ mlAuto <+> "fas fa-chevron-right" ]
    []
  ]
