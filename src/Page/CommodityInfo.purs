module Yzmall.Page.CommodityInfo where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (maybeElem, safeHref)
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (preview)
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import Partial.Unsafe (unsafePartial)
import Slug (Slug, generate)
import String (banner1, bgCommodity, bgInfoTitle, c105Info)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Avatar (Avatar, parse)
import Yzmall.Data.Commodity (Commodity, CommodityCategory(..), forkData)
import Yzmall.Data.Route (Route(..))
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Part.Navbar as Navbar
import Yzmall.Page.Utils (mobileOnly, pcOnly, yellowColor)
import Yzmall.Resource.Commodity (class ManageCommodity, getCommodity)
import Yzmall.Utils (CardInfo, cls, foreach_, renderBanner, renderCommodity, renderFooter, renderHeader, renderNavBar, style, (->>))

type Input = 
  { slug :: Slug }

type State =
  { commodity :: RemoteData String Commodity
  , slug :: Slug
  }

data Query a 
  = Initialize a
  | GetCommodity a
  | PurchaseCommodity Int a

type ChildQuery = Navbar.Query
type ChildSlot = Unit

component
  :: forall m r
   . MonadAff m
  => ManageCommodity m
  => ManageAccount m
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
  initialState { slug } = 
    { commodity: NotAsked
    , slug
    }

  eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
  eval = case _ of
    Initialize a -> do
      void $ H.fork $ eval $ GetCommodity a
      pure a
    GetCommodity a -> do
      st <- H.modify _ { commodity = Loading }
      commodity <- getCommodity st.slug
      H.modify_ _ { commodity = fromMaybe commodity }
      pure a
    PurchaseCommodity _ a -> 
      pure a 
  
  render :: State -> H.ParentHTML Query ChildQuery Unit m
  render state = 
     HH.div
        [ cls $ containerFluid <> px0 
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"]
        [ renderHeader
        , maybeElem mbCommodity renderNavbar'
        -- | show pc
        , maybeElem mbCommodity pcDiv
        -- | show on mobile
        , maybeElem mbCommodity mobileDiv
        , HH.div
          [ cls $ container <> px0 <> dFlex <> flexColumn <> h100]
          [ HH.img
            [ HP.src bgInfoTitle
            , cls $ w100 <> py2
            ]
          , maybeElem mbCommodity info
          ]
        , renderFooter
        , maybeElem mbCommodity purchaseBtn
        ]
      where
      mbCommodity = preview _Success state.commodity

      renderNavbar' commodity = 
        HH.slot unit Navbar.component { page: (if Special == commodity.category then Second else First)  } absurd

      purchaseBtn commodity = 
        HH.div
          [ cls mobileOnly ]
          [ HH.a 
            [ cls $ btn <> btnDanger <> w100 <> fixedBottom
            , style "font-size: 24px"
            , safeHref $ (if commodity.category == Regular then PurchaseConfirm else SpecialPurchaseConfirm) $ unsafePartial $ fromJust $ generate $ show commodity.id
            ]
            [ HH.text "立即购买"]
          ]

      info commodity =
        HH.img 
        [ cls w100 
        , HP.src commodity.picture
        ]
      
      mobileDiv commodity =
        HH.div
          [cls $ container <> px0 <> mobileOnly <> flexColumn <> mxAuto]
          [ HH.img
            [ HP.src commodity.thumbnail 
            , cls mw100
            ]
          , HH.div 
            [ cls $ bgDanger <> dBlock <> px1 <> mb2]
            [ HH.div 
              [ cls $ floatLeft <> textWhite <> h3 <> my1 ]
              [ HH.text $ "￥" <> show commodity.price <> "元"]
            , HH.div
              [ cls $ floatRight <> mt2
              , style "color: #f8f2d8"
              ]
              [ HH.text $ "（" <> (if commodity.category == Regular then "可获得" else "需消耗") <> show commodity.gold <> "金币）" ]
            ]
          , HH.div
            [ cls $  row <> container <> px0 <> mxAuto]
            [ HH.div 
              [ cls $ col12 <> px1 ]
              [ HH.h4 
                [ style "color: #f8f2d8" ]
                [ HH.text $ commodity.name ]
              , HH.div
                [ cls $ w100 <> pt2 <> textWhite <> flexColumn
                , style "border-top: 1px dashed #f8f2d8"
                ]
                [ HH.div 
                  [ cls $ mb1 <> dFlex]
                  [ HH.div_ [HH.text "运费: 包邮" ]
                  , HH.div 
                    [ cls  mlAuto ]
                    [ HH.text $ "销量: " <> show commodity.sale ]
                  ]
                , HH.div 
                  [ cls $ w100 <> dFlex ]
                  [ HH.div 
                    [ cls mlAuto]
                    [ HH.text $ "库存: " <> show commodity.stock ]
                  ]
                ]
              
              ]
            ]
          ]

      pcDiv commodity =
        HH.div
          [ cls $ row <> container <> mxAuto <> px0 <> mt2 <> pcOnly
          , style "height:470px"
          ]
          [ HH.div 
            [cls $ colMd8 <> col12 <> pl0 <> h100
            , style $ "background-image: url(" <> commodity.thumbnail <> "); background-position: center center; background-size: cover"
            ]
            []
          , HH.div 
            [ cls $ colMd4 <> pr0 <> dFlex <> flexColumn <> h100
            ]
            [ HH.h3
              [ cls textWhite ]
              [ HH.text commodity.name ]
            , HH.h2 
              [ style yellowColor ]  
              [ HH.text $ "￥" <> show commodity.price <> "元" ]
            , HH.h5
              [ style yellowColor ]
              [ HH.text $ "("<> (if commodity.category == Regular then "可获得" else "需消耗") <> show commodity.gold <> "金币)"]
            , HH.div
              [ cls $ dFlex <> flexColumn <> textWhite <> pt2 <> mbAuto
              , style "border-top: 1px dashed #f8f2d8"
              ]
              [ HH.div 
                [ cls $ mb4 ]
                [ HH.text "运费: 包邮" ]
              , HH.div 
                [ cls mb4]
                [ HH.text $ "销量: " <> show commodity.sale ]
              , HH.div_
                [ HH.text $ "库存: " <> show commodity.stock ]
              ]
            , HH.a
              [ cls $ btn <> btnDanger <> w100 <> rounded0 <> pt0
              , style "font-size: 38px" 
              , safeHref $ (if commodity.category == Regular then PurchaseConfirm else SpecialPurchaseConfirm) $ unsafePartial $ fromJust $ generate $ show commodity.id ]
              [ HH.div 
                [ cls myAuto ]
                [ HH.text "立即购买" ]
              ]
            ]
          ]

