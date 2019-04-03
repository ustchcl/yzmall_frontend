module Yzmall.Page.Part.Navbar where 
  
import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (safeHref)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.String (length)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Timer (IntervalId, clearInterval, setInterval)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import String (alipayNote, logo, logoNavbar)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount, bindAlipay, createAccount, createVcode, login, resetPassword)
import Yzmall.Capability.Navigate (class Navigate, navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Route (HomeType(..), Route(..))
import Yzmall.Data.Route2 as R2
import Yzmall.Page.Part.Login (loginFields, registerFields, renderLoginModal, renderRegisterModal)
import Yzmall.Page.Utils (alertMsg, closeBindAlipayModal, closeLoginModal, closeRegisterModal, getInputValue, renderInputGroup, renderInputGroupPassword, timerDown)
import Yzmall.Utils (cls, renderModal, style, (->>))

data NavbarPage
  = First 
  | Second
  | Third
  | Fourth

derive instance  eqNavbarPage :: Eq NavbarPage

type LoginForm = 
  { phone :: String
  , password :: String
  }
type RegisterForm = 
  { phone :: String
  , password :: String
  }

data GetVcodeState 
  = BtnAvailable
  | BtnDisable
  | BtnGrey

derive instance eqGetVcodeState :: Eq GetVcodeState 

type State = 
  { currentAccount :: Maybe Account
  , vcodeBtnState :: GetVcodeState
  , vcodeBtnState2 :: GetVcodeState
  , intervalId :: Maybe IntervalId
  , page :: NavbarPage     
  }

type Input = 
  { page :: NavbarPage }

data Query a
  = Initialize a
  | Reset Boolean a
  | LoginAction a
  | RegisterAction a
  | GetVCode a
  | GetVCode_Reset a
  | ResetPassword a 
  | CheckPhoneNumber a
  | CheckPhoneNumber2 a
  | BindAliPay a


component 
  :: forall m r
   . MonadAff m 
   => MonadAsk { currentAccount :: Ref (Maybe Account) | r} m
   => Navigate m
   => ManageAccount m
   => H.Component HH.HTML Query Input Void m
component =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where
  initialState { page } = 
    { currentAccount: Nothing
    , vcodeBtnState: BtnDisable
    , vcodeBtnState2: BtnDisable
    , intervalId: Nothing
    , page 
    }
  
  render :: State -> H.ComponentHTML Query
  render state@{currentAccount, vcodeBtnState, vcodeBtnState2, page} = 
    HH.div_
    [ renderNavBar state 
    , renderModal "loginModal" "登录" footer body true
    , renderModal "registerModal" "注册" footer2 body2 true 
    , renderModal "resetPasswordModal" "重置密码" footer3 body3 true 
    , renderModal "bindAlipayModal" "绑定支付宝" footer4 body4 false
    ]
    where
    footer =
      [ 
        HH.button
        [ cls $ btn <> btnDanger <> btnBlock <> w25 <> mr3 <> mlAuto
        , "type" ->> "button"
        , HE.onClick $ HE.input_ $ LoginAction
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
          [ renderInputGroup loginPhone 
          , renderInputGroupPassword loginPassword
          
          ]
        ]
      , HH.div
            [ cls $ w100 <> dFlex ]
            [ HH.a 
              [ cls $ mr3 <> mlAuto
              , "data-dismiss" ->> "modal"
              , "data-toggle" ->> "modal"
              , "data-target" ->> "#resetPasswordModal" 
              ]
              [ HH.text "忘记密码" ]
            ]
      ]

    loginPhone = 
        { id : "phone"
        , icon : "fas fa-mobile-alt"
        , placeholder: "输入手机号"
        , required : true
        }
    loginPassword = 
        { id : "password"
        , icon : "fas fa-unlock-alt"
        , placeholder: "输入密码（6 ~ 16位）"
        , required : true
        }

    body2 = 
      [ HH.div
        [cls $ dFlex <> justifyContentCenter <> alignItemsCenter] 
        [ HH.img
          [ style "max-height: 150px"
          , HP.src logo ]
        ]
      , HH.form_ 
        [ HH.div
          [ cls formGroup ]
          [ HH.div 
            [ cls $ inputGroup <> px3 <> mb2]
            [ HH.div
              [ cls $ inputGroupPrepend ]
              [ HH.span
                [ "id" ->> r1.id <> "-prepend" 
                , cls $ inputGroupText <> dFlex <> justifyContentCenter
                , style "width: 50px"
                ]
                [ HH.i 
                  [ "class" ->> r1.icon ]
                  []
                ]
              ]
              , HH.input 
                [ "type" ->> "text"
                , cls $ formControl
                , "id" ->> "validation-" <> r1.id
                , HP.placeholder r1.placeholder
                , "value" ->> ""
                , HP.required r1.required
                , HP.readOnly true
                ]             
              , HH.div 
                [ cls invalidFeedback ]
                [ HH.text "" ]
              ]
          , renderInputGroup r2
          , HH.div 
            [ cls $ inputGroup <> px3 <> mb2]
            [ HH.div
              [ cls $ inputGroupPrepend ]
              [ HH.span
                [ "id" ->> r3.id <> "-prepend" 
                , cls $ inputGroupText <> dFlex <> justifyContentCenter
                , style "width: 50px"
                ]
                [ HH.i 
                  [ "class" ->> r3.icon ]
                  []
                ]
              ]
              , HH.input 
                [ "type" ->> "text"
                , cls $ formControl
                , "id" ->> "validation-" <> r3.id
                , HP.placeholder r3.placeholder
                , "value" ->> ""
                , HP.required r3.required
                , HE.onInput $ HE.input_ CheckPhoneNumber
                ]             
              , HH.div 
                [ cls invalidFeedback ]
                [ HH.text "" ]
              ]
          , HH.div 
            [ cls $ inputGroup <> px3 <> mb2]
            [ HH.div
              [ cls $ inputGroupPrepend ]
              [ HH.span
                [ "id" ->> r4.id <> "-prepend" 
                , cls $ inputGroupText <> dFlex <> justifyContentCenter
                , style "width: 50px"
                ]
                [ HH.i 
                  [ "class" ->> r4.icon ]
                  []
                ]
              ]
              , HH.input 
                [ "type" ->> "text"
                , cls $ formControl
                , "id" ->> "validation-" <> r4.id
                , HP.placeholder r4.placeholder
                , "value" ->> ""
                , HP.required r4.required
                ]
              , HH.button 
                [ "type" ->> "button"
                , cls $ btn <> btnDanger <> ml2
                , HE.onClick $ HE.input_ GetVCode 
                , HP.disabled $ vcodeBtnState /= BtnAvailable
                ]
                [ HH.text $ case vcodeBtnState of
                    BtnAvailable -> "获取"
                    BtnDisable -> "获取"
                    BtnGrey -> "已发送"  
                ]                
              , HH.div 
                [ cls invalidFeedback ]
                [ HH.text "" ]
              ]
          , renderInputGroupPassword r5
          , renderInputGroupPassword r6
          ]
        ]
      ]

    footer2 = 
      [ HH.div
        [ cls $ dFlex <> ml3 <> floatLeft]
        [ HH.div_ [ HH.text "已有账号?现在" ]
        , HH.a
          [ safeHref (RegularCommodity NormalHome) ]
          [ HH.text "去登录" ]
        ]
      , HH.button
        [ cls $ btn <> btnDanger <> btnBlock <> w25 <> mr3 <> mlAuto
        , "type" ->> "button"
        , HE.onClick $ HE.input_ RegisterAction
        ]
        [ HH.text "注册" ]
      ]
    body3 = 
      [ HH.div
        [cls $ dFlex <> justifyContentCenter <> alignItemsCenter] 
        [ HH.img
          [ style "max-height: 150px"
          , HP.src logo ]
        ]
      , HH.form_ 
        [ HH.div
          [ cls formGroup ]
          [ HH.div 
            [ cls $ inputGroup <> px3 <> mb2]
            [ HH.div
              [ cls $ inputGroupPrepend ]
              [ HH.span
                [ "id" ->> r3'.id <> "-prepend" 
                , cls $ inputGroupText <> dFlex <> justifyContentCenter
                , style "width: 50px"
                ]
                [ HH.i 
                  [ "class" ->> r3'.icon ]
                  []
                ]
              ]
              , HH.input 
                [ "type" ->> "text"
                , cls $ formControl
                , "id" ->> "validation-" <> r3'.id
                , HP.placeholder r3'.placeholder
                , "value" ->> ""
                , HP.required r3'.required
                , HE.onInput $ HE.input_ CheckPhoneNumber2
                ]             
              , HH.div 
                [ cls invalidFeedback ]
                [ HH.text "" ]
              ]
          , HH.div 
            [ cls $ inputGroup <> px3 <> mb2]
            [ HH.div
              [ cls $ inputGroupPrepend ]
              [ HH.span
                [ "id" ->> r4'.id <> "-prepend" 
                , cls $ inputGroupText <> dFlex <> justifyContentCenter
                , style "width: 50px"
                ]
                [ HH.i 
                  [ "class" ->> r4'.icon ]
                  []
                ]
              ]
              , HH.input 
                [ "type" ->> "text"
                , cls $ formControl
                , "id" ->> "validation-" <> r4'.id
                , HP.placeholder r4'.placeholder
                , "value" ->> ""
                , HP.required r4'.required
                ]
              , HH.button 
                [ "type" ->> "button"
                , cls $ btn <> btnDanger <> ml2
                , HE.onClick $ HE.input_ GetVCode_Reset
                , HP.disabled $ vcodeBtnState2 /= BtnAvailable
                ]
                [ HH.text $ case vcodeBtnState2 of
                    BtnAvailable -> "获取"
                    BtnDisable -> "获取"
                    BtnGrey -> "已发送"  
                ]              
              , HH.div 
                [ cls invalidFeedback ]
                [ HH.text "" ]
              ]
          , renderInputGroupPassword r5'
          , renderInputGroupPassword r6'
          ]
        ]
      ]

    footer3 = 
      [ HH.button
        [ cls $ btn <> btnDanger <> btnBlock <> w25 <> mr3 <> mlAuto
        , "type" ->> "button"
        , "data-dismiss" ->> "modal"
        , HE.onClick $ HE.input_ ResetPassword
        ]
        [ HH.text "修改密码" ]
      ]

  
    r1 =
        { id : "inviter"
        , icon : "fa fa-user-plus"
        , placeholder: "输入邀请者ID"
        , required : false
        }
    r2 =
        { id : "nickname"
        , icon : "fa fa-user-alt"
        , placeholder: "输入昵称"
        , required : true
        }
    r3 =
        { id : "phone-register"
        , icon : "fa fa-mobile-alt"
        , placeholder: "输入手机号"
        , required : true
        }
    r4 =
        { id : "vcode"
        , icon : "fa fa-comments"
        , placeholder: "输入验证码"
        , required : true
        }
    r5 =
        { id : "password1"
        , icon : "fa fa-unlock"
        , placeholder: "输入密码（6 ~ 16位）"
        , required : true
        }
    r6 = 
        { id : "password2"
        , icon : "fa fa-unlock-alt"
        , placeholder: "确认密码（6 ~ 16位）"
        , required : true
        }

    r3' =
        { id : "phone-resetPassword"
        , icon : "fa fa-mobile-alt"
        , placeholder: "输入手机号"
        , required : true
        }
    r4' =
        { id : "vcode-resetPassword"
        , icon : "fa fa-comments"
        , placeholder: "输入验证码"
        , required : true
        }
    r5' =
        { id : "password1-resetPassword"
        , icon : "fa fa-unlock"
        , placeholder: "输入密码（6 ~ 16位）"
        , required : true
        }
    r6' = 
        { id : "password2-resetPassword"
        , icon : "fa fa-unlock-alt"
        , placeholder: "确认密码（6 ~ 16位）"
        , required : true
        }

    body4 = 
      [ HH.div
        [cls $ dFlex <> justifyContentCenter <> alignItemsCenter] 
        [ HH.img
          [ style "max-height: 150px"
          , HP.src logo ]
        ]
      , HH.form_ 
        [ HH.div
          [ cls formGroup ]
          [ renderInputGroup aliPayInput 
          , renderInputGroup aliPayInput2
          , HH.div 
            [ cls $ dFlex <> flexColumn <> px3  ]
            [ HH.p 
              [ cls $ fontWeightBold <> my2 ]
              [ HH.text "重要提示！"]
            , HH.p
              [cls $ fontWeightNormal <> mb0 ] 
              [HH.text alipayNote]
            ]
          ]
        ]
      ]

    aliPayInput = 
        { id : "alipay"
        , icon : "fab fa-alipay fa-2x"
        , placeholder: "输入支付宝账号"
        , required : true
        }
    aliPayInput2 = 
        { id : "alipay2"
        , icon : "fab fa-alipay fa-2x"
        , placeholder: "确认支付宝账号"
        , required : true
        }

    footer4 = 
      [ 
        HH.button
        [ cls $ btn <> btnDanger <> btnBlock <> w25 <> mr3 <> mlAuto
        , "type" ->> "button"
        , HE.onClick $ HE.input_ $ BindAliPay
        ]
        [ HH.text "绑定"]
      ]
  

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of 
    Initialize a -> do
      { currentAccount } <- ask
      account <- liftEffect $ Ref.read currentAccount
      H.modify_ _ { currentAccount = account}
      pure a 
    Reset isRegular a -> do
      H.modify_ _ { page = if isRegular then First else Second}
      pure a
    LoginAction a -> do
      phone <- liftEffect $ getInputValue "validation-phone"
      password <- liftEffect $ getInputValue "validation-password"
      account <- login { phone: phone, password: password }
      { currentAccount } <- ask
      liftEffect $ Ref.write account currentAccount
      case account of 
        Just acc -> do 
          liftEffect $ alertMsg "登录成功"
        Nothing -> 
          pure unit
      H.modify_ _ { currentAccount = account }
      liftEffect $ closeLoginModal
      pure a
    RegisterAction a -> do
      inviter <- liftEffect $ getInputValue "validation-inviter"
      nickname <- liftEffect $ getInputValue "validation-nickname"
      phone <- liftEffect $ getInputValue "validation-phone-register"
      vcode <- liftEffect $ getInputValue "validation-vcode"
      password1 <- liftEffect $ getInputValue "validation-password1"
      password2 <- liftEffect $ getInputValue "validation-password2"
      case inviter /= "" of 
        true -> 
          case password2 == password1 of 
            true -> do 
              account <- createAccount { inviter, nickname, phone, vcode, password: password1 }
              when (isJust account) do
                liftEffect $ alertMsg "注册成功，已自动登录"
              { currentAccount } <- ask
              liftEffect $ Ref.write account currentAccount
              H.modify_ _ { currentAccount = account }
              liftEffect $ closeRegisterModal
            false -> 
              liftEffect $ alertMsg "密码不一致"
        false -> 
          liftEffect $ alertMsg "邀请者不可为空"
      pure a
    GetVCode a -> do
      phone <- liftEffect $ getInputValue "validation-phone-register"
      H.modify_ _ {vcodeBtnState = BtnGrey}
      -- let 
      --   cb = do
      --     st <- H.get
      --     case st.vcodeBtnState of
      --       BtnGrey 0 -> do
      --         liftEffect $ clearInterval (unsafePartial $ fromJust st.intervalId)
      --         void $ H.fork $ eval $ CheckPhoneNumber a
      --       BtnGrey n -> do
      --         H.modify_ _ {vcodeBtnState = BtnGrey (n - 1)}
      --       _ -> pure unit
      --     pure unit
      -- intevalId <- liftEffect $ setInterval 1000 $ drive
      -- H.modify_ _ {intevalId = Just intevalId}
      _ <- createVcode phone
      pure a
    GetVCode_Reset a -> do
      phone <- liftEffect $ getInputValue "validation-phone-resetPassword"
      H.modify_ _ {vcodeBtnState2 = BtnGrey}
      _ <- createVcode phone
      pure a
    ResetPassword a -> do
      phone <- liftEffect $ getInputValue "validation-phone-resetPassword"
      vcode <- liftEffect $ getInputValue "validation-vcode-resetPassword"
      password1 <- liftEffect $ getInputValue "validation-password1-resetPassword"
      password2 <- liftEffect $ getInputValue "validation-password2-resetPassword"

      
      case password2 == password1 of 
        true -> do 
          account <- resetPassword {phone, vcode, password: password1 }
          { currentAccount } <- ask
          liftEffect $ Ref.write account currentAccount
          case account of 
            Just _ -> do 
              liftEffect $ alertMsg "密码修改成功"
            Nothing -> 
              pure unit
          H.modify_ _ { currentAccount = account }
        false -> 
          liftEffect $ alertMsg "密码不一致"
      pure a     
    CheckPhoneNumber a -> do
      phone <- liftEffect $ getInputValue "validation-phone-register"
      st <- H.get
      case length phone == 11 of 
        true -> do 
          if (st.vcodeBtnState == BtnDisable) then H.modify_ _ {vcodeBtnState = BtnAvailable} else pure unit 
        false -> do
          if (st.vcodeBtnState == BtnAvailable) then H.modify_ _ {vcodeBtnState = BtnDisable} else pure unit 
      pure a
    CheckPhoneNumber2 a -> do
      phone <- liftEffect $ getInputValue "validation-phone-resetPassword"
      st <- H.get
      case length phone == 11 of 
        true -> do 
          if (st.vcodeBtnState2 == BtnDisable) then H.modify_ _ {vcodeBtnState2 = BtnAvailable} else pure unit 
        false -> do
          if (st.vcodeBtnState2 == BtnAvailable) then H.modify_ _ {vcodeBtnState2 = BtnDisable} else pure unit 
      pure a
    BindAliPay a -> do 
      alipay <- liftEffect $ getInputValue "validation-alipay"
      confrimAlipay <- liftEffect $ getInputValue "validation-alipay2"
      case alipay /= "" of 
        true ->
          case alipay == confrimAlipay of 
            true -> do 
              account <- bindAlipay alipay
              { currentAccount } <- ask
              liftEffect $ Ref.write account currentAccount
              H.modify_ _ { currentAccount = account }
              when (isJust account) do 
                liftEffect $ closeBindAlipayModal
                liftEffect $ alertMsg "支付宝账号绑定成功"
            false -> do
              liftEffect $ alertMsg "支付宝账号不一致"
        false -> 
          liftEffect $ alertMsg "支付宝账号不可为空"
      pure a

renderNavBar :: ∀ p i. State -> HH.HTML p i
renderNavBar st = 
  HH.nav 
  [ cls  $ navbar <> navbarExpandMd <> navbarDark <>  bgTransparent <> H.ClassName "sticky-top" ]
  [ HH.div
    [ cls $ container <> px0]
    [ HH.a 
      [ cls $ navbarBrand <> dFlex <> alignItemsCenter
      , safeHref (RegularCommodity NormalHome)
      ]
      [ HH.img
        [ HP.src logoNavbar
        , style "width: 131px; height: 29px"]
      ]
    , HH.button 
      [ cls navbarToggler
      , "data-toggle" ->> "collapse"
      , "data-target" ->> "#navbarNav"
      , "type" ->> "button"
      ]
      [ HH.span [cls navbarTogglerIcon ] [] ]
    , HH.div
      [ cls $ collapse <> navbarCollapse 
      , "id" ->> "navbarNav" 
      ] 
      ([ HH.ul
        [ cls navbarNav ]
        [ renderNavItem  "正价商品" First (RegularCommodity NormalHome) 
        -- , renderNavItem  "特价商品" Second (SpecialCommodity NormalHome) 
        , renderNavItem  "交易中心" Third TradeCenter 
        , renderNavItem  "个人中心" Fourth (PC_ROUTER Nothing) 
        ]
      ] <> (if isJust st.currentAccount then [] else btns) )
    ]
  ]
  
  where
  btns = 
    [ HH.button
      [ cls $ btn <> btnOutlineDanger <> my2 <> mlAuto <> H.ClassName "my-sm-0"
      , "data-toggle" ->> "modal"
      , "data-target" ->> "#loginModal"
      ]
      [ HH.text "登录" ]
    -- , HH.button
    --   [ cls $ btn <> btnDanger <> my2 <> ml3 <> H.ClassName "my-sm-0"
    --   , "data-toggle" ->> "modal"
    --   , "data-target" ->> "#registerModal" 
    --   ]
    --   [ HH.text "注册"]
    ]

  renderNavItem str n route =
    HH.li
    [ cls $ navItem <> (if n == st.page then active else H.ClassName "")]
    [ HH.a
      [ cls $ navLink 
      , safeHref route
      ]
      [ HH.text str ]
    ]