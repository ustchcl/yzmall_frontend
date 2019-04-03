module Yzmall.Page.Utils where

import Halogen.Themes.Bootstrap4
import Prelude
import Prelude

import Conduit.Component.Utils (safeHref, whenElem)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Yzmall.Data.Commodity (Commodity)
import Yzmall.Data.Route (Route)
import Yzmall.Utils (cls, renderModal, style, (->>), (<+>))


-- | 输入框
type InputGroupConfig = 
  { id :: String
  , icon :: String
  , placeholder :: String
  , required :: Boolean
  }

renderInputGroup :: forall p i. InputGroupConfig -> H.HTML p i 
renderInputGroup config = 
    HH.div 
    [ cls $ inputGroup <> px3 <> mb2]
    [ HH.div
      [ cls $ inputGroupPrepend ]
      [ HH.span
        [ "id" ->> config.id <> "-prepend" 
        , cls $ inputGroupText <> dFlex <> justifyContentCenter
        , style "width: 50px"
        ]
        [ HH.i 
          [ "class" ->> config.icon ]
          []
        ]
      ]
      , HH.input 
        [ "type" ->> "text"
        , cls $ formControl
        , "id" ->> "validation-" <> config.id
        , HP.placeholder config.placeholder
        , "value" ->> ""
        , HP.required config.required
        ]
      , HH.div 
        [ cls invalidFeedback ]
        [ HH.text "" ]
      ]

renderInputGroupPassword :: forall p i. InputGroupConfig -> H.HTML p i 
renderInputGroupPassword config = 
    HH.div 
    [ cls $ inputGroup <> px3 <> mb2]
    [ HH.div
      [ cls $ inputGroupPrepend ]
      [ HH.span
        [ "id" ->> config.id <> "-prepend" 
        , cls $ inputGroupText <> dFlex <> justifyContentCenter
        , style "width: 50px"
        ]
        [ HH.i 
          [ "class" ->> config.icon ]
          []
        ]
      ]
      , HH.input 
        [ cls $ formControl
        , "id" ->> "validation-" <> config.id
        , HP.placeholder config.placeholder
        , "value" ->> ""
        , HP.required config.required
        , "type" ->> "password"
        ]
      , HH.div 
        [ cls invalidFeedback ]
        [ HH.text "" ]
      ]

pcOnly :: H.ClassName
pcOnly = 
  dNone <> H.ClassName "d-md-flex"

mobileOnly :: H.ClassName
mobileOnly = 
  dFlex <> H.ClassName "d-md-none"

yellowColor :: String
yellowColor = 
  "color: #F6C17F;"

lightYellowColor :: String
lightYellowColor = 
  "color: #f8f2d8;"

existWhenZero :: Int -> H.ClassName -> H.ClassName
existWhenZero n cn = (if n == 0 then cn else H.ClassName "")

greyBg :: String
greyBg = "background-color: #414040;"

greyColor :: String
greyColor = 
  "color: #adadad;"

darkRedColor :: String 
darkRedColor = "color: #ff8181;"

lightBlueColor :: String 
lightBlueColor = "color: #B6F9FF;"

type BarInfo = 
  { name :: String
  , rightDesc :: Maybe String
  , leftIcon :: Maybe String
  , withRightIcon :: Boolean
  }

renderBar :: forall p i. BarInfo -> Maybe Route -> H.HTML p i
renderBar  {name, rightDesc, leftIcon, withRightIcon} route =
  HH.a
  [ cls $ listGroupItem <> dFlex <> alignItemsCenter <> px3 <> rounded0 <> listGroupItemDark <> listGroupItemAction <> textWhite
  , (if isJust route then safeHref $ unsafePartial $ fromJust route else "nothing" ->> "nothing")
  , style "font-size: 15px"
  ]
  [ 
    HH.div
    [ cls $ dFlex <> flexRow <> alignItemsCenter]
    [ leftImg leftIcon
    , HH.div_ [ HH.text name]
    ]
  , renderRight rightDesc
  ]
  where
  leftImg (Just src) = 
    HH.div 
    [ cls mr1 ]
    [ HH.img 
      [ HP.src src
      , style "height: 27px; width: 27px" 
      ]
    ]
  leftImg Nothing = HH.text "" 

  rightIcon true = 
    HH.i
    [ cls $ ml2 <+> "fas fa-chevron-right" ]
    []
  rightIcon false =  HH.text ""

  renderRight str =
    case str of
      Just v ->
        HH.div
        [ cls mlAuto]
        [ HH.text v
        , rightIcon withRightIcon
        ]
      Nothing ->
      HH.div
        [ cls mlAuto]
        [ rightIcon withRightIcon ]

--- js 
foreign import openBindAlipayModal :: Effect Unit
foreign import closeBindAlipayModal :: Effect Unit
foreign import closeLoginModal :: Effect Unit
foreign import closeRegisterModal :: Effect Unit
foreign import openLoginModal :: Effect Unit
foreign import openRegisterModal :: String -> Effect Unit
foreign import getInputValue :: String -> Effect String
foreign import callback :: forall m. (m Unit) -> Effect Unit
foreign import alertMsg :: String -> Effect Unit
unsafeAlert :: String -> Unit
unsafeAlert = unsafePerformEffect <<< alertMsg
foreign import getRadioBtnChecked :: String -> Effect Boolean


foreign import initCommodities :: Effect Unit
foreign import getCommodityById :: Int -> Commodity
foreign import getInviterId :: Effect String
foreign import showProv :: Effect Unit
foreign import getTimerCurrent :: Effect Int
foreign import setTimer :: Int -> Effect Unit
foreign import timerDown :: Effect Unit

foreign import setNumOnly :: String -> Effect Unit