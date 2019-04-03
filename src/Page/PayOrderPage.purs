module Yzmall.Page.PayOrderPage where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (maybeElem)
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.State (state)
import Data.Lens (preview)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import String (bgCommodity, sharePng)
import Type.Data.Boolean (kind Boolean)
import Yzmalal.Data.CommodityOrder (CommodityOrder)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount, createVcode)
import Yzmall.Api.Capablity.Resource.Address (class ManageAddress)
import Yzmall.Api.Capablity.Resource.BankCard (class ManageBankCard, getBankCard)
import Yzmall.Api.Capablity.Resource.CommodityOrder (class ManageOrder, payForOrder, payForOrderSpecial)
import Yzmall.Capability.Navigate (class Navigate, navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.BankCard (BankCard)
import Yzmall.Data.Route (Route(..))
import Yzmall.Data.Route2 as Route2
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu, renderBackBtn)
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Part.Navbar as Navbar
import Yzmall.Page.Utils (alertMsg, getInputValue)
import Yzmall.Resource.Commodity (class ManageCommodity)
import Yzmall.Utils (CardInfo, cls, renderBanner, renderFooter, renderHeader, renderNavBar, style, unsafeSlug, (->>))



data PayBtnState 
  = Uploading 
  | OJBK

derive instance eqPayBtnState :: Eq PayBtnState

type State = 
  { order :: RemoteData String CommodityOrder
  , isRegular :: Boolean
  , currentAccount :: Maybe Account
  , bankCard :: RemoteData String BankCard
  , commodityId :: Int
  , priceDesc :: String
  , commodityName :: String 
  , orderId :: Int
  , cardId :: Int
  , showVcode :: Maybe Unit
  , payBtnState :: PayBtnState
  }

type Input = 
  { isRegular :: Boolean
  , commodityId :: Int
  , priceDesc :: String
  , commodityName :: String 
  , orderId :: Int
  , cardId :: Int
  }

data Query a 
  = Initialize a
  | GetBankCard a
  | GetOrder a
  | GetVcode a

type ChildQuery = 
  Navbar.Query
  
type ChildSlot = Unit

component
  :: forall m r
   . MonadAff m
  => ManageCommodity m
  => ManageAccount m
  => ManageOrder m
  => ManageAddress m
  => ManageBankCard m
  => MonadAsk { currentAccount :: Ref (Maybe Account) | r } m
  => Navigate m
  => H.Component HH.HTML Query Input Void m
component =
  H.lifecycleParentComponent
      { initialState 
      , render
      , eval 
      , receiver: const Nothing
      , initializer: Just $ H.action Initialize
      , finalizer: Nothing
      }
  where
  initialState :: Input -> State
  initialState { isRegular, commodityId, priceDesc, commodityName, orderId , cardId } = 
    { order : NotAsked
    , isRegular
    , currentAccount : Nothing
    , bankCard : NotAsked
    , showVcode : Nothing
    , commodityId
    , priceDesc
    , commodityName
    , orderId
    , cardId
    , payBtnState: OJBK
    }

  eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
  eval = case _ of
    Initialize next -> do
      void $ H.fork $ eval $ GetBankCard next
      pure next
    GetBankCard next -> do
      _ <- H.modify _ { bankCard = Loading }
      st <- H.get
      bankCard <- getBankCard $ unsafeSlug (show st.cardId)
      H.modify_ _ { bankCard = fromMaybe bankCard }
      pure next
    GetOrder a -> do
      st <- H.get
      vcode <- liftEffect $ getInputValue "pay_input"
      let 
        payFunc = if st.isRegular then payForOrder else payForOrderSpecial
        params = if (isJust st.showVcode) then (Just vcode) else Nothing
      
      H.modify_ _ {payBtnState = Uploading}
      result <- payFunc (unsafeSlug $ show st.orderId) params
      H.modify_ _ {payBtnState = OJBK}
      case result of 
        (Just r) -> 
          case r.expectVcode of
            true -> do 
              H.modify_ _ { showVcode = Just unit }
            false -> do
              liftEffect $ alertMsg "商品抢购成功"
              navigate $ PC_ROUTER (Just Route2.MyOrders)
        Nothing -> 
          liftEffect $ alertMsg "支付请求失败"

      pure a

    GetVcode a -> do 
      st <- H.get
      case st.bankCard of 
        (Success bc) -> do
          _ <- createVcode bc.phone
          pure unit
        _ -> do
          liftEffect $ alertMsg "银行卡信息获取失败"
          pure unit 
      pure a
  
  render :: State -> H.ParentHTML Query ChildQuery Unit  m
  render state = 
      HH.div
        [ cls $ containerFluid <> px0 
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"
        ]
        [ renderHeader
        , HH.slot unit Navbar.component { page: page } absurd
        , HH.div
          [ cls $ container <> px0 <> textWhite
          , style "min-height: 633px" 
          ]
          [ HH.div 
            [ cls $ w100 <> py2 <> dFlex <> justifyContentCenter
            , style "background-color: #303030"
            ] 
            [ HH.img
              [ style "max-width: 260px; max-height: 260px"
              , HP.src "./image/pay_banner.png"
              ]
            ]
          , HH.div 
            [ cls $ dBlock <> mt3 <> w100 <> px3 ]
            [ HH.text $ "商品名称: " <> state.commodityName ]
          , HH.div 
            [ cls $ dBlock <> mt3 <> w100 <> px3 ]
            [ HH.text $ "价格: " <> state.priceDesc ]
          , maybeElem state.showVcode vcodeInput
          , HH.div 
            [ cls $ w100 <> px3] 
            [ HH.button 
              [ cls $ dBlock <> w100 <> py2 <> btn <> btnDanger <> mt3 <> mb3
              , HE.onClick $ HE.input_ GetOrder
              , HP.disabled $ state.payBtnState == Uploading
              ]
              [ HH.text $ case state.payBtnState of 
                  OJBK -> if (isJust state.showVcode) then "输入验证码后确认" else  "确认购买"
                  Uploading -> "正在支付..."
              ]
            ]
          ]
        , renderFooter
        , forMobileMenu
        , mobileMenu page
        ]
    where
    page = if (state.isRegular) then First else Second

    vcodeInput _ = 
      HH.div 
      [ cls $ inputGroup <> px3 <> mt3 ]
      [ HH.input 
        [ "type" ->> "text"
        , cls $ formControl 
        , "placeholder" ->> "输入验证码"
        , "id" ->> "pay_input"
        ]
      -- , HH.button 
      --   [ cls $ btn <> btnDanger <> ml2 
      --   , "type" ->> "button"
      --   , HE.onClick $ HE.input_ GetVcode 
      --   ]
      --   [ HH.text "获取" ]
      ]  