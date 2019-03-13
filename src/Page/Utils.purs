module Yzmall.Page.Utils where

import Halogen.Themes.Bootstrap4
import Prelude

import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Yzmall.Utils (cls, renderModal, style, (->>))


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