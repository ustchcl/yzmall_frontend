module Yzmall.Page.Commodity where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (safeHref)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array (filter, sortBy)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), fromMaybe)
import String (banner1, banner2, bgBanner, bgCommodity, bgTuiGuang, cardContent, iconWdds, iconWdfx, iconWdtg, imgUrl, rectPng)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Avatar (Avatar, parse)
import Yzmall.Data.Commodity (Commodity, CommodityCategory(..))
import Yzmall.Data.Route (HomeType(..), Route(..))
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Part.Navbar as Navbar
import Yzmall.Page.Utils (closeLoginModal, closeRegisterModal, getInviterId, openBindAlipayModal, openLoginModal, openRegisterModal)
import Yzmall.Resource.Commodity (class ManageCommodity, getCommodities)
import Yzmall.Utils (CardInfo, cls, foreach_, renderBanner, renderCommodity, renderFooter, renderHeader, renderNavBar, style, (->>))

type State = 
  { currentAccount :: Maybe Account
  , commodities :: RemoteData String (Array Commodity)
  , isRegular :: Boolean
  , hometype :: HomeType
  }

type Input = 
  { isRegular :: Boolean
  , hometype :: HomeType 
  }

data Query a 
  = Initialize a
  | LoadCommodities Boolean a
  | Reset Boolean HomeType a

type ChildQuery = Navbar.Query
type ChildSlot = Unit

component 
  :: forall m r
   . MonadAff m 
   => MonadAsk { currentAccount :: Ref (Maybe Account) | r} m
   => Navigate m
   => ManageCommodity m
   => ManageAccount m
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
    initialState { isRegular, hometype} = 
      { currentAccount : Nothing
      , commodities : NotAsked
      , isRegular
      , hometype
      }

    render :: State -> H.ParentHTML Query ChildQuery Unit m
    render state =
        HH.div
        [ cls $ containerFluid <> px0
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"]
        [ renderHeader
        , HH.slot unit Navbar.component { page: if state.isRegular then First else Second } absurd
        , HH.div
          [ cls $ w100 <> py2
          , style $ "background-image: url(" <> bgBanner <> ")" ]
          [ HH.div
            [ cls $ container <> px0 ]
            [ renderBanner ]
          ]
        -- , renderMenu
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
        , mobileMenu $ if state.isRegular then First else Second
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
            foreach_ (sortBy (\a b -> if b.id - a.id > 0 then GT else LT) $ (filter (\x -> x.onSale) arr)) renderCommodity

    eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
    eval = case _ of
      Initialize a -> do
        st <- H.get
        void $ H.fork $ eval $ LoadCommodities st.isRegular a
        {currentAccount} <- ask
        account <- liftEffect $ Ref.read currentAccount
        when (st.hometype == BindAlipay) do 
          liftEffect $ closeRegisterModal *> closeLoginModal *> openBindAlipayModal
        when (isNothing account) do 
            case st.hometype of 
              NormalHome -> pure unit
              LoginHome -> liftEffect $ closeRegisterModal *> openLoginModal
              RegisterHome -> do 
                inviterId <- liftEffect $ getInviterId
                liftEffect $ closeLoginModal 
                liftEffect $ openRegisterModal inviterId
              _ -> pure unit
        
        pure a
      LoadCommodities isRegular a -> do
        st <- H.modify _ { commodities = Loading }
        commodities <- getCommodities { category: show (if isRegular then Regular else Special),  page: Nothing, size: Nothing }
        H.modify_ _ { commodities = fromMaybe commodities }
        pure a
      Reset isRegular hometype a -> do
        H.put { currentAccount : Nothing
              , commodities : NotAsked
              , isRegular
              , hometype
              }
        _ <- H.query unit (H.action $ Navbar.Reset isRegular)
        void $ H.fork $ eval $ Initialize a
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
