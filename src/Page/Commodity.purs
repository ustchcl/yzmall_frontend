module Yzmall.Page.Commodity where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (safeHref)
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), fromMaybe)
import String (banner1, banner2, bgBanner, bgCommodity, bgTuiGuang, cardContent, iconWdds, iconWdfx, iconWdtg, imgUrl, rectPng)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Avatar (Avatar, parse)
import Yzmall.Data.Commodity (Commodity, CommodityCategory(..))
import Yzmall.Data.Route (Route(..))
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Resource.Commodity (class ManageCommodity, getCommodities)
import Yzmall.Utils (CardInfo, cls, foreach_, renderBanner, renderCommodity, renderFooter, renderHeader, renderNavBar, style, (->>))

type State = 
  { currentAccount :: Maybe Account
  , commodities :: RemoteData String (Array Commodity)
  }

data Query a 
  = Initialize a
  | LoadCommodities a

component 
  :: forall m r
   . MonadAff m 
   => MonadAsk { currentAccount :: Ref (Maybe Account) | r} m
   => Navigate m
   => ManageCommodity m
   => H.Component HH.HTML Query Unit Void m
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
    initialState :: Unit -> State
    initialState _ = 
      { currentAccount : Nothing
      , commodities : NotAsked
      }

    render :: State -> H.ComponentHTML Query
    render state =
        HH.div
        [ cls $ containerFluid <> px0
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"]
        [ renderHeader
        , renderNavBar
        , HH.div
          [ cls $ w100 <> py2
          , style $ "background-image: url(" <> bgBanner <> ")" ]
          [ HH.div
            [ cls $ container <> px0 ]
            [ renderBanner [banner1, banner2] ]
          ]
        , renderMenu
        , HH.div
          [ cls $ bgTransparent <> w100]
          [ HH.div
            [ cls $ container <> px0 ]
            [ HH.div
              [ cls $ dFlex <> flexWrap ]
              (renderCommodities state.commodities)
            ]
          ]
        , renderFooter
        , forMobileMenu
        , mobileMenu
        , renderLoginModal
        , renderRegisterModal
        ]

        where
        cardInitialState :: CardInfo
        cardInitialState =
            { title: "霸王翡翠"
            , content: cardContent
            , imgSrc: imgUrl
            , btnName: "Go somewhere"
            }
        
        renderCommodities = case _ of
          NotAsked ->  
            [ HH.text "commodities not loaded" ]
          Loading -> 
            [ HH.text "Loading commodities" ]
          Failure err ->  
            [ HH.text $ "Failed loading commodities: " <> err ]
          Success arr ->
            foreach_ arr renderCommodity

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of
      Initialize a -> do
        void $ H.fork $ eval $ LoadCommodities a
        pure a
      LoadCommodities a -> do
        st <- H.modify _ { commodities = Loading }
        commodities <- getCommodities { category: show Regular, page: Nothing, size: Nothing }
        H.modify_ _ { commodities = fromMaybe commodities }
        pure a

renderMenu :: forall p i. H.HTML p i
renderMenu =
    HH.div
    [ cls $ w100
    , style $ "background-image: url(" <> bgTuiGuang <> "); background-position: top center;"
    ]
    [ HH.div
      [ cls container
      , "style" ->> "height: 120px"
      ]
      [ HH.div
        [ cls $ row <> alignItemsCenter <> h100 ]
        [ _renderImg 1 iconWdfx WDFX_ROUTE
        , _renderImg 2 iconWdtg WDTG_ROUTE
        , _renderImg 3 iconWdds WDDS_ROUTE
        ]
      ]
    ]
    where
    _renderImg n imgSrc route =
      let bordern = if (n == 1) then borderRight <> borderLeft else borderRight
      in
        HH.a
        [ cls $ col4 <> bordern <> mt2
        , safeHref route
        , style $ "height: 100px; background-image: url(" <> imgSrc <> ");background-repeat: no-repeat; background-size: contain ; background-position: center center"
        ]
        []
