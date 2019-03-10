module Yzmall.Page.Part.Login where

import Halogen.Themes.Bootstrap4
import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import String (logo)
import Yzmall.Utils (cls, renderModal, style, (->>))

renderLoginModal :: ∀ p i. H.HTML p i 
renderLoginModal = 
  renderModal "loginModal" "登录" footer body
  where
  -- body :: ∀ p i. Array (H.HTML p i)
  footer =
    [ HH.a
      [ cls $ ml3 <> floatLeft
      , HP.href "#"]
      [ HH.text "立即注册" ]
      
    , HH.button
      [ cls $ btn <> btnDanger <> btnBlock <> w25 <> mr3 <> mlAuto
      , "type" ->> "button"
      ]
      [ HH.text "登录"]
    ]

  body = 
    [ HH.div
      [cls $ dFlex <> justifyContentCenter <> alignItemsCenter] 
      [ HH.img
        [ style "max-height: 200px"
        , HP.src logo ]
      ]
    , HH.form_ 
      [ HH.div
        [ cls formGroup ]
        [ HH.div
          [ cls $ inputGroup <> mb2 <> px3]
          [ HH.div
            [ cls inputGroupPrepend ]
            [ HH.span
              [ "id" ->> "phonePrepend" 
              , cls $ inputGroupText <> dFlex <> justifyContentCenter
              , style "width: 50px"
              ]
              [ HH.i 
                [ "class" ->> "fas fa-mobile-alt" ]
                []
              ]
            ]
          , HH.input 
            [ "type" ->> "text"
            , cls $ formControl <> isValid
            , "id" ->> "validationPhone"
            , HP.placeholder "输入手机号"
            , "value" ->> ""
            , HP.required true
            ]
          ]
        , HH.div 
          [ cls invalidFeedback ]
          [ HH.text "" ]
        , HH.div 
          [ cls $ inputGroup <> px3]
          [ HH.div
            [ cls $ inputGroupPrepend ]
            [ HH.span
              [ "id" ->> "passwordPrepend" 
              , cls $ inputGroupText <> dFlex <> justifyContentCenter
              , style "width: 50px"
              ]
              [ HH.i 
                [ "class" ->> "fas fa-unlock-alt" ]
                []
              ]
            ]
          , HH.input 
            [ "type" ->> "text"
            , cls $ formControl <> isValid
            , "id" ->> "validationPassword"
            , HP.placeholder "输入密码（6 ~ 16位）"
            , "value" ->> ""
            , HP.required true
            ]
          ]
        , HH.div 
          [ cls invalidFeedback ]
          [ HH.text "" ]
        ]
      ]
    , HH.div
      [ cls $ w100 <> dFlex ]
      [ HH.a 
        [ cls $ mr3 <> mlAuto
        , HP.href "#"]
        [ HH.text "忘记密码" ]
      ]
    ]