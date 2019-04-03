module Yzmall.Page.Part.MobileMenu where

import Halogen.Themes.Bootstrap4
import Prelude

import Conduit.Component.Utils (safeHref)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import String (accountIcon, accountIconM, logo, recordIcon, recordIconM, regularIcon, regularIconM, specialIcon, specialIconM)
import Yzmall.Data.Route (HomeType(..), Route(..))
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Utils (InputGroupConfig, mobileOnly, renderInputGroup)
import Yzmall.Utils (cls, renderModal, style, (->>), (<+>))

type MenuConfig =
  { iconSrc :: String
  , selectedIconSrc :: String 
  , name :: String
  , href :: String
  }

m1 =
   { iconSrc : regularIcon
    , selectedIconSrc : regularIconM
    , name : "正价区"
    , href : "#"
    }
m2 =
   { iconSrc : specialIcon
    , selectedIconSrc : specialIconM
    , name : "特价区"
    , href : "#"
    }
m3 =
   { iconSrc : recordIcon
    , selectedIconSrc : recordIconM
    , name : "交易中心"
    , href : "#"
    }
m4 =
   { iconSrc : accountIcon
    , selectedIconSrc : accountIconM
    , name : "个人中心"
    , href : "#"
    }


forMobileMenu :: forall p i. H.HTML p i
forMobileMenu =
  HH.div
  [ cls $ dBlock <> H.ClassName "d-md-none" <> bgTransparent
  , style "height: 70px"
  ]
  []

mobileMenu :: forall p i. NavbarPage -> H.HTML p i
mobileMenu page = 
  HH.div
  [ cls $ btnGroup  <> dBlock <> H.ClassName "d-md-none" <> dFlex <> justifyContentBetween <> fixedBottom
  , "role" ->> "group"]
  [ renderMenuItem page First m1 (RegularCommodity NormalHome)
  -- , renderMenuItem page Second m2 (SpecialCommodity NormalHome)
  , renderMenuItem page Third m3 TradeCenter
  , renderMenuItem page Fourth m4 (PC_ROUTER Nothing) 
  ]


renderMenuItem :: forall p i. NavbarPage -> NavbarPage -> MenuConfig -> Route -> H.HTML p i
renderMenuItem page1 page2 {iconSrc, selectedIconSrc, name, href} route =
  HH.a
  [ cls $ btn <> btnDark <> dFlex <> flexColumn <> alignItemsCenter <+> (if page1 == page2 then "menuItemActive" else "menuItemInactive")
  , safeHref route
  , style "width: 33.3333%"]
  [ HH.div 
    [ cls $ dFlex <> flexColumn ]
    [ HH.img
      [ HP.src selectedIconSrc
      , style "width: 32px; height: 32px"
      , cls $ mxAuto
      ]
    , HH.p
      [ style "color: #F3DDB4"
      , cls m0]
      [ HH.text name ]
    ]
  ]

renderBackBtn :: forall p i. H.HTML p i
renderBackBtn = 
  HH.button 
  [ cls $ btn <> btnDanger <> roundedCircle <> mobileOnly
  , style "position: fixed; top: 35px; left: 15px; z-index: 999; height: 36px; width: 36px; z-index: 999" 
  , "onclick" ->> "javascript:history.back(-1);" 
  ]
  [ HH.i 
    [ cls $ H.ClassName "fas fa-chevron-left" ]
    []
  ]