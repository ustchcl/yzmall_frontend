module Yzmall.Page.Wdtg where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Affjax.RequestBody (RequestBody(..))
import Data.Array (replicate)
import Data.Maybe (Maybe(..))
import Formless (initial)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import String (banner1, bgCommodity, bgInfoTitle, c105Info)
import Yzmall.Data.Avatar (Avatar, parse)
import Yzmall.Data.Commodity (Commodity, CommodityCategory(..), forkData)
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Page.Utils (mobileOnly, pcOnly, yellowColor)
import Yzmall.Utils (CardInfo, cls, foreach_, renderBanner, renderCommodity, renderFooter, renderHeader, renderNavBar, style, (->>))

type Record = 
  { iconSrc :: String
  , date :: String
  , price :: Number
  , name :: String 
  , amount :: Int
  , vipLevel :: String 
  }

type State = Array Record

data Query a = 
    InitWdtgInfo a

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
      { initialState: const initialState 
      , render
      , eval 
      , receiver: const Nothing
      }
  where
  initialState :: State
  initialState = 
    replicate 8 { iconSrc : "https://randomuser.me/api/portraits/women/60.jpg"
    , date: "2019-03-11"
    , price: 2.0
    , name: "鱼龙潜跃观道身"
    , amount: 1
    , vipLevel: "VIP会员"
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    InitWdtgInfo next -> 
      pure next
  
  render :: State -> H.ComponentHTML Query
  render state = 
      HH.div
        [ cls $ containerFluid <> px0 
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"]
        [ renderHeader
        , renderNavBar
        , HH.div
          [cls $ container <> flexColumn <> mxAuto <> mt2
          , style "min-height: 633px" ]
          (renderRecord <$> state)
        , renderFooter
        , forMobileMenu
        , mobileMenu
        , renderLoginModal
        , renderRegisterModal
        ]
      where 
      renderRecord record = 
        HH.div
        [ cls $ dBlock <> textWhite <> mb1 <> bgTransparent <> borderBottom <> px0
        , style "height: 72px; font-size: 15px; border-bottom-color: #555555!important" ]
        [ HH.div 
          [ cls $ row <> mw100 <> mx0]
          [ HH.img 
              [ cls $ colMd1 <> col3 <> myAuto <> dBlock <> px0 <> mxAuto
              , HP.src record.iconSrc
              , style "max-width: 60px; max-height: 60px;"
              ]
            , HH.div
              [ cls $ colMd11 <> col9 <> dFlex <> flexColumn <> alignContentBetween ]
              [ HH.div_ [ HH.text $ record.name <> " （" <> record.vipLevel <> "）" ]
              , HH.div_ [ HH.text record.date ]
              , HH.div_ [ HH.text $ "推广 " <> show record.amount ]
              ]
          ]
        ]