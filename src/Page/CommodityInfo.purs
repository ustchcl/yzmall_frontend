module Yzmall.Page.CommodityInfo where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (maybeElem)
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (preview)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import Slug (Slug)
import String (banner1, bgCommodity, bgInfoTitle, c105Info)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Avatar (Avatar, parse)
import Yzmall.Data.Commodity (Commodity, CommodityCategory(..), forkData)
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
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

component
  :: forall m r
   . MonadAff m
  => ManageCommodity m
  => ManageAccount m
  => MonadAsk { currentAccount :: Ref (Maybe Account) | r } m
  => Navigate m
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
  initialState :: Input -> State
  initialState { slug } = 
    { commodity: NotAsked
    , slug
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
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
  
  render :: State -> H.ComponentHTML Query
  render state = 
     HH.div
        [ cls $ containerFluid <> px0 
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"]
        [ renderHeader
        , renderNavBar
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
        , HH.div
          [ cls mobileOnly
          , style "height: 50px"
          ]
          []
        , HH.div
          [ cls mobileOnly ]
          [ HH.button 
            [ cls $ btn <> btnDanger <> w100 <> fixedBottom
            , style "font-size: 24px"
            ]
            [ HH.text "立即购买"]
          ]
        , renderLoginModal
        , renderRegisterModal
        ]
      where
      mbCommodity = preview _Success state.commodity

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
              [ HH.text $ "（需消耗" <> show commodity.gold <> "金币）" ]
            ]
          , HH.div
            [ cls $  row <> container <> px0 <> mxAuto]
            [ HH.div 
              [ cls $ col7 <> px1 ]
              [ HH.h4 
                [ style "color: #f8f2d8" ]
                [ HH.text $ commodity.name ]
              , HH.div
                [ cls $ w100 <> pt2 <> textWhite
                , style "border-top: 1px dashed #f8f2d8"
                ]
                [ HH.div 
                  [ cls $ mb1 ]
                  [ HH.text "运费: 包邮" ]
                , HH.div 
                  [ cls mb1]
                  [ HH.text $ "销量: " <> show commodity.sale ]
                , HH.div_
                  [ HH.text $ "库存: " <> show commodity.stock ]
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
              [ HH.text $ "("<> (if commodity.category == Regular then "需消耗" else "可获得") <> show commodity.gold <> "金币)"]
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
            , HH.button
              [ cls $ btn <> btnDanger <> w100 <> rounded0 <> pt0
              , style "font-size: 38px" ]
              [ HH.div 
                [ cls myAuto ]
                [ HH.text "立即购买" ]
              ]
            ]
          ]

