module Yzmall.Page.Part.MobileMenu where

import Halogen.Themes.Bootstrap4
import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import String (accountIcon, accountIconM, logo, recordIcon, recordIconM, regularIcon, regularIconM, specialIcon, specialIconM)
import Yzmall.Page.Utils (InputGroupConfig, renderInputGroup)
import Yzmall.Utils (cls, renderModal, style, (->>))

type MenuConfig =
  { iconSrc :: String
  , selectedIconSrc :: String 
  , name :: String
  , href :: String
  }

menuConfigs :: Array MenuConfig
menuConfigs = 
  [ { iconSrc : regularIcon
    , selectedIconSrc : regularIconM
    , name : "正价区"
    , href : "#"
    }
  , { iconSrc : specialIcon
    , selectedIconSrc : specialIconM
    , name : "特价区"
    , href : "#"
    }
  , { iconSrc : recordIcon
    , selectedIconSrc : recordIconM
    , name : "交易中心"
    , href : "#"
    }
  , { iconSrc : accountIcon
    , selectedIconSrc : accountIconM
    , name : "个人中心"
    , href : "#"
    }
  ]


forMobileMenu :: forall p i. H.HTML p i
forMobileMenu =
  HH.div
  [ cls $ dBlock <> H.ClassName "d-md-none" <> bgTransparent
  , style "height: 70px"
  ]
  []

mobileMenu :: forall p i. H.HTML p i
mobileMenu = 
  HH.div
  [ cls $ btnGroup <> btnGroupToggle <> dBlock <> H.ClassName "d-md-none" <> dFlex <> justifyContentBetween <> fixedBottom
  , "data-toggle" ->> "buttons"]
  (renderMenuItem <$> menuConfigs)


renderMenuItem :: forall p i. MenuConfig -> H.HTML p i
renderMenuItem {iconSrc, selectedIconSrc, name, href} =
  HH.div
  [ cls $ btn <> btnDark <> dFlex <> flexColumn <> alignItemsCenter <> w25]
  [ HH.div 
    [ cls $ dFlex <> flexColumn]
    [ HH.img
      [ HP.src selectedIconSrc
      , style "width: 32px; height: 32px"
      , cls $ mxAuto
      ]
    , HH.p
      [ style "color: #FC5106"
      , cls m0]
      [ HH.text name ]
    ]
  , HH.input
    [ "type" ->> "radio"
    , "autocomplete" ->> "off"
    ]
  
  ]
