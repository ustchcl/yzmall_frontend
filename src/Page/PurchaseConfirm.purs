module Yzmall.Page.PurchaseConfirm where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (guardAccount, maybeElem, safeHref)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array (head)
import Data.Lens (preview)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 (btnDanger, textWhite)
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import Slug (Slug, toString)
import String (bgCommodity)
import Yzmalal.Data.CommodityOrder (CommodityOrder)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount, getAccountInfo)
import Yzmall.Api.Capablity.Resource.Address (class ManageAddress, getAddress, myAddresses)
import Yzmall.Api.Capablity.Resource.BankCard (class ManageBankCard, getBankCard, myBankCards)
import Yzmall.Api.Capablity.Resource.CommodityOrder (class ManageOrder, CreateOrderParams, createOrder, createOrderSpecial)
import Yzmall.Capability.Navigate (class Navigate, logout, navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Address (Address)
import Yzmall.Data.BankCard (BankCard)
import Yzmall.Data.Commodity (Commodity, CommodityCategory(..))
import Yzmall.Data.Route (HomeType(..), Route(..))
import Yzmall.Data.Route2 (Route2(..))
import Yzmall.Page.BankCardEditor as BCE
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu, renderMenuItem)
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Part.Navbar as Navbar
import Yzmall.Page.Utils (alertMsg, greyBg, greyColor, mobileOnly, pcOnly, renderBar, yellowColor)
import Yzmall.Resource.Commodity (class ManageCommodity, getCommodity)
import Yzmall.Utils (backgroundImage, cls, encodePhone, renderFooter, renderHeader, renderNavBar, style, unsafeSlug, (->>), (<+>))

type Input = 
  { slug :: Slug
  , isSpecial :: Boolean
  }

type State = 
  { account :: Maybe Account
  , address :: RemoteData String Address 
  , bankCard :: RemoteData String BankCard 
  , commodity :: RemoteData String Commodity
  , order :: RemoteData String CommodityOrder
  , slug :: Slug
  , isSpecial :: Boolean
  }


data Query a
  = Initialize a 
  | GetCommodity a
  | GetAddress a
  | GetBankCard a
  | ConfirmPurchase Int a
  | CreateOrder a 

type ChildQuery = Navbar.Query
type ChildSlot = Unit

component
  :: forall m r
   . MonadAff m
  => ManageCommodity m
  => ManageAccount m
  => ManageOrder m
  => ManageAddress m
  => ManageBankCard m
  => MonadAsk { currentAccount :: Ref (Maybe Account), currentAddressId :: Ref (Maybe Int),  currentBankCardId :: Ref (Maybe Int), lastRoute :: Ref (Maybe Route) | r } m
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
  initialState { slug, isSpecial } = 
    { account : Nothing
    , address : NotAsked 
    , bankCard: NotAsked
    , commodity : NotAsked
    , order : NotAsked
    , slug
    , isSpecial
    }

  render :: State -> H.ParentHTML Query ChildQuery Unit m
  render state = 
    HH.div
      [ cls $ containerFluid <> px0 
      , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"
      ]
      [ renderHeader
      , maybeElem mbCommodity renderNavbar'
      -- | show pc
      , HH.div 
        [ cls $ container <> px0 <> mxAuto
        , style "min-height: 633px"
        ]
        [ renderAddress mbAddress
        , renderBankCard mbBankCard
        , maybeElem mbCommodity commodityInfo
        , maybeElem state.account (renderSpecial state.isSpecial)
        , maybeElem mbCommodity confirmPc
        ]
      , maybeElem mbCommodity confirmMobile
      , renderFooter
      , mobileMenu (if Just Special == (_.category <$> mbCommodity) then Second else First)
      , forMobileMenu
      ]
    where
      renderNavbar' commodity = 
        HH.slot unit Navbar.component { page: (if Special == commodity.category then Second else First)  } absurd

      renderSpecial true account =
        let menuItemInfo = 
                    { name : "可用金币"
                    , rightDesc : Just $ show account.gold
                    , leftIcon : Nothing
                    , withRightIcon: false
                    }
        in 
          HH.div
          [ cls mt3 ]
          [ HH.div_
            [ renderBar menuItemInfo Nothing ]
          , HH.div
            [ cls mt3  ]
            [ renderBar { name : "", rightDesc : Just "平台代售", leftIcon: Nothing, withRightIcon: false } Nothing]
          ]

      renderSpecial false _ = 
        HH.text ""

      confirmMobile commodity =
        HH.div
        [ style greyBg 
        , cls $ w100 <> row <> px0 <> flexRowReverse <> mobileOnly <> mt3 <> mx0 <> H.ClassName "payment-bar"
        ]
        [ HH.div
          [ cls $ col4 <> colMd2 <> px0 ]
          [ HH.a
            [ cls $ btn <> btnDanger <> textWhite <> rounded0 <> w100 <> h100 <> dFlex ]
            [ HH.div 
              [ cls $ myAuto <> mxAuto
              , HE.onClick $ HE.input_ CreateOrder 
              ] 
              [ HH.text "确认支付" ]
            ]
          ]
        , HH.div 
          [ cls $ col8 <> colMd4 <> pl2 <> dFlex <> flexRow <> myAuto
          , style yellowColor
          ]
          [ HH.div
            [ cls $ mr2 ]
            [ HH.text $ "实付款: " <> show commodity.price <> "元"] 
          , HH.div_ [ HH.text $ "金币: " <> show commodity.gold ]   
          ]
        ]

      confirmPc commodity = 
        HH.div
        [ style $ greyBg <> "height: 50px"
        , cls $ w100 <> row <> px0 <> flexRowReverse <> pcOnly <> mt3 <> mx0
        ]
        [ HH.div
          [ cls $ col4 <> colMd2 <> px0 ]
          [ HH.a
            [ cls $ btn <> btnDanger <> textWhite <> rounded0 <> w100 <> h100 <> dFlex 
            , HE.onClick $ HE.input_ CreateOrder]
            [ HH.div 
              [ cls $ myAuto <> mxAuto ] 
              [ HH.text "确认支付" ]
            ]
          ]
        , HH.div 
          [ cls $ col8 <> colMd4 <> pl2 <> dFlex <> flexRow <> myAuto
          , style yellowColor
          ]
          [ HH.div
            [ cls $ mr2 <> mlAuto ]
            [ HH.text $ "实付款: " <> show commodity.price <> "元"] 
          , HH.div_ [ HH.text $ (if commodity.category == Regular then "赠" else "") <> "消耗金币: " <> show commodity.gold ]   
          ]
        ]

      mbCommodity = preview _Success state.commodity
      mbAddress = preview _Success state.address
      mbBankCard = preview _Success state.bankCard
      {-
        maybeElem (Just x) f = f x
        maybeElem _ _ = HH.text "
      -}
      renderAddress (Just addr) = 
        HH.a
        [ cls $ py2 <> px2 <> w100 <> mb3 <> dBlock <> dFlex <> alignItemsCenter
        , style $  greyBg <> "text-decoration:none;"
        , safeHref $ PC_ROUTER $ Just AddressSelector
        ] 
        [ HH.div
          [ cls $ dFlex <> flexColumn ]
          [ HH.div
            [ cls textWhite]
            [ HH.div 
              [ cls mr1 ]
              [ HH.text addr.name]
            , HH.div_
              [ HH.text $ encodePhone addr.phone ]
            ]
          , HH.div
            [ cls $ textWhite <> dFlex <> alignItemsCenter ]
            [ HH.i 
              [ cls $ mr1 <> H.ClassName "fas fa-map-marker-alt" ]
              []
            , HH.div_ [HH.text $ addr.address]
            ]
          ]
        , HH.div 
          [ cls $ textDanger <> mlAuto ]
          [ HH.text "更换地址"]
        ]   

      renderAddress _ = 
        let menuItemInfo = 
              { name : "请添加地址"
              , rightDesc : Nothing
              , leftIcon : Nothing
              , withRightIcon: true
              }
        in 
        HH.div 
        [ cls mb3 ]
        [ renderBar menuItemInfo $ (Just $ PC_ROUTER $ Just AddressEditor) ]
      
      renderBankCard (Just bankCard) = 
        HH.a
        [ cls $ py2 <> px2 <> w100 <> mb3 <> dBlock <> dFlex <> alignItemsCenter
        , style $  greyBg <> "text-decoration:none;"
        , safeHref $ PC_ROUTER $ Just BankCardSelector
        ] 
        [ HH.div
          [ cls $ dFlex <> flexColumn ]
          [ HH.div
            [ cls textWhite]
            [ HH.div 
              [ cls mr1 ]
              [ HH.text bankCard.name]
            , HH.div_
              [ HH.text $ encodePhone bankCard.phone ]
            ]
          , HH.div
            [ cls $ textWhite <> dFlex <> alignItemsCenter ]
            [ HH.text bankCard.cardId ]
          ]
        , HH.div 
          [ cls $ textDanger <> mlAuto ]
          [ HH.text "换卡支付"]
        ]   

      renderBankCard _ = 
        let menuItemInfo = 
              { name : "请添加银行卡"
              , rightDesc : Nothing
              , leftIcon : Nothing
              , withRightIcon: true
              }
        in 
        HH.div 
        [ cls mb3 ]
        [ renderBar menuItemInfo $ (Just $ PC_ROUTER $ Just BankCardEditor) ]


      commodityInfo commodity =
        HH.div 
        [ cls $ w100 <> dFlex <> py2 <> mx0 <> px3
        , style $ greyBg  <> "height: 86px"
        ]
        [ HH.div
          [ style $ "background-image: url(" <> commodity.thumbnail <> "); background-position: center left; background-size: cover; background-repeat: no-repeat; " <> "width: 120px; height: 70px" ]
          []
        , HH.div 
            [ cls $ dFlex <> flexColumn <> justifyContentBetween <> textWhite <> w100 <> pl2
            , style "height: 70px"]
            [ HH.div_ [ HH.text commodity.name ]
            , HH.div
              [ cls $ dFlex <> flexRow <> mtAuto ]
              [ HH.div 
                [style yellowColor
                , cls mr2
                ]
                [ HH.text $ "￥" <> show commodity.price <> "元" ]
              , HH.div 
                [ style yellowColor ]
                [ HH.text $ "金币: " <> show commodity.gold ]
              , HH.div
                [ style greyColor
                , cls mlAuto ]
                [ HH.text "数量: 1" ]
              ]
            ]
        ]


  
  eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
  eval = case _ of
    Initialize a -> do
      void $ H.fork $ eval $ GetCommodity a
      guardAccount >>= case _ of
        Nothing -> pure unit
        _ -> do
          account <- getAccountInfo
          H.modify_ _ { account = account }
          void $ H.fork $ eval $ GetAddress a
          void $ H.fork $ eval $ GetBankCard a
      pure a
    GetCommodity a -> do
      st <- H.modify _ { commodity = Loading }
      commodity <- getCommodity  st.slug  
      H.modify_ _ { commodity = fromMaybe commodity }
      pure a
    GetAddress a -> do
      st <- H.get
      { currentAddressId } <- ask
      addressId <- liftEffect $ Ref.read currentAddressId
      case combineMaybe addressId (_.defaultAddress =<< st.account) of
        Nothing -> do
          _ <- H.modify _ { address = Loading }
          addresses <- myAddresses
          H.modify_ _ { address = fromMaybe $ head =<< addresses }
        (Just addrId) -> do
          _ <- H.modify _ { address = Loading }
          address <- getAddress (unsafeSlug $ show addrId)
          H.modify_ _ { address = fromMaybe address }
      
      pure a
    GetBankCard a -> do
      st <- H.get
      { currentBankCardId } <- ask
      bankCardId <- liftEffect $ Ref.read currentBankCardId
      case combineMaybe bankCardId (_.defaultBankCard =<< st.account) of
        Nothing -> do
          _ <- H.modify _ { bankCard = Loading }
          bankCards <- myBankCards
          H.modify_ _ { bankCard = fromMaybe $ head =<< bankCards }
        (Just bId) -> do
          _ <- H.modify _ { bankCard = Loading }
          bankCard <- getBankCard (unsafeSlug $ show bId)
          H.modify_ _ { bankCard = fromMaybe bankCard }
      
      pure a
    ConfirmPurchase _ a -> 

      pure a
    CreateOrder  a -> do
      st <- H.get
      let 
        mbAddressId = _.id <$> preview _Success st.address
        mbCommodity = preview _Success st.commodity
        mbBankCard = preview _Success st.bankCard
        mbParams = params mbAddressId
        createFunc = if st.isSpecial then createOrderSpecial else createOrder
      mbOrder <- createFunc st.slug mbParams
      case mbOrder of 
        Just order -> 
          navigate $ PayOrder 
            { isRegular: not st.isSpecial
            , commodityId: getCId mbCommodity
            , priceDesc: getPriceDesc mbCommodity
            , commodityName: getCName mbCommodity
            , orderId: order.id 
            , cardId: getBankCardId mbBankCard
            }
        Nothing -> 
          pure unit
      pure a

getPriceDesc :: Maybe Commodity -> String 
getPriceDesc Nothing = ""
getPriceDesc (Just c) =
  case c.category of 
    Regular -> show c.price <> "元"
    Special -> show c.price <> "元 " <> show c.gold <> "金币"

getCName :: Maybe Commodity -> String 
getCName Nothing = ""
getCName (Just c) = c.name

getCId :: Maybe Commodity -> Int 
getCId Nothing = -1
getCId (Just c) = c.id

getBankCardId :: Maybe BankCard -> Int 
getBankCardId Nothing = -1
getBankCardId (Just c) = c.id

combineMaybe :: forall a. Maybe a -> Maybe a -> Maybe a
combineMaybe (Just a) _  = (Just a)
combineMaybe Nothing b = b

params :: Maybe Int -> CreateOrderParams
params addrId = 
  { tag : 0
  , addressId : addrId
  , amount: 1
  }