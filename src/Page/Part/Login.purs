module Yzmall.Page.Part.Login where

import Halogen.Themes.Bootstrap4
import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import String (logo)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount, login)
import Yzmall.Data.Account (Account)
import Yzmall.Page.Utils (InputGroupConfig, callback, getInputValue, renderInputGroup)
import Yzmall.Utils (cls, renderModal, style, (->>))


registerFields :: Array InputGroupConfig
registerFields = 
  [ { id : "inviter"
    , icon : "fa fa-user-plus"
    , placeholder: "输入邀请者手机号"
    , required : false
    }
  , { id : "nickname"
    , icon : "fa fa-user-alt"
    , placeholder: "输入昵称"
    , required : true
    }
  , { id : "phone"
    , icon : "fa fa-mobile-alt"
    , placeholder: "输入手机号"
    , required : true
    }
  , { id : "validate-code"
    , icon : "fa fa-comments"
    , placeholder: "输入验证码"
    , required : true
    }
  , { id : "password1"
    , icon : "fa fa-unlock"
    , placeholder: "输入密码"
    , required : true
    }
  , { id : "password2"
    , icon : "fa fa-unlock-alt"
    , placeholder: "确认密码"
    , required : true
    }
  ]

renderRegisterModal :: ∀ p i. H.HTML p i
renderRegisterModal = 
  renderModal "registerModal" "注册" footer body true
  where
    footer = 
      [ HH.div
        [ cls $ dFlex <> ml3 <> floatLeft]
        [ HH.div_ [ HH.text "已有账号?现在" ]
        , HH.a
          [ HP.href "#"
          , "data-dismiss" ->> "modal"
          , "data-toggle" ->> "modal"
          , "data-target" ->> "#loginModal"]
          [ HH.text "去登录" ]
        ]
      
    , HH.button
      [ cls $ btn <> btnDanger <> btnBlock <> w25 <> mr3 <> mlAuto
      , "type" ->> "button"
      ]
      [ HH.text "下一步"]
    ]

    body = 
      [ HH.div
      [cls $ dFlex <> justifyContentCenter <> alignItemsCenter] 
      [ HH.img
        [ style "max-height: 150px"
        , HP.src logo ]
      ]
    , HH.form_ 
      [ HH.div
        [ cls formGroup ]
        (renderInputGroup <$> registerFields)
      ]
    ]


loginFields :: Array InputGroupConfig
loginFields = 
  [ { id : "phone"
    , icon : "fas fa-mobile-alt"
    , placeholder: "输入手机号"
    , required : true
    }
  , { id : "password"
    , icon : "fas fa-unlock-alt"
    , placeholder: "输入密码（6 ~ 16位）"
    , required : true
    }
  ]
renderLoginModal :: ∀ p i. H.HTML p i 
renderLoginModal = 
  renderModal "loginModal" "登录" footer body true
  where
  -- body :: ∀ p i. Array (H.HTML p i)
  footer =
    [ HH.a
      [ cls $ ml3 <> floatLeft
      , "data-toggle" ->> "modal"
      , "data-target" ->> "#registerModal"
      , "data-dismiss" ->> "modal"
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
        [ style "max-height: 150px"
        , HP.src logo ]
      ]
    , HH.form_ 
      [ HH.div
        [ cls formGroup ]
        (renderInputGroup <$> loginFields)
      ]
    , HH.div
      [ cls $ w100 <> dFlex ]
      [ HH.a 
        [ cls $ mr3 <> mlAuto
        , HP.href "#"]
        [ HH.text "忘记密码" ]
      ]
    ]


-- loginAction
--   :: forall m r
--    . MonadEffect m
--   => MonadAsk { currentAccount :: Ref (Maybe Account) | r } m
--   => ManageAccount m
--   => String -> String -> m Unit
-- loginAction phone password =
--   -- account <- login { phone, password }
--   login { phone, password } >>= \account -> do
--     liftEffect $ Ref.write account $ asks _.currentAccount
--     pure unit

loginAction 
  :: forall m r
   . MonadEffect m 
  => MonadAsk { currentAccount :: Ref (Maybe Account) | r } m
  => ManageAccount m
  => String -> String -> m Unit
loginAction phone password = do
  account <- login { phone, password }
  { currentAccount } <- ask
  liftEffect $ Ref.write account currentAccount