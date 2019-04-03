module Yzmall.Page.Wdfx where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Control.Monad.Reader (class MonadAsk)
import Control.Monad.State (state)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Formless (initial)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import String (bgCommodity, sharePng)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount)
import Yzmall.Api.Capablity.Resource.Address (class ManageAddress)
import Yzmall.Api.Capablity.Resource.CommodityOrder (class ManageOrder)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Part.Navbar as Navbar
import Yzmall.Resource.Commodity (class ManageCommodity)
import Yzmall.Utils (CardInfo, cls, renderBanner, renderFooter, renderHeader, renderNavBar, style, (->>))

type InviterID = Int

type State = Int

data Query a = 
    Initialize a

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
  => MonadAsk { currentAccount :: Ref (Maybe Account) | r } m
  => Navigate m
  => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleParentComponent
      { initialState: const initialState 
      , render
      , eval 
      , receiver: const Nothing
      , initializer: Just $ H.action Initialize
      , finalizer: Nothing
      }
  where
  initialState :: State
  initialState = 1770724

  eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
  eval = case _ of
    Initialize next -> 
      pure next
  
  render :: State -> H.ParentHTML Query ChildQuery Unit  m
  render state = 
      HH.div
        [ cls $ containerFluid <> px0 
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"
        ]
        [ renderHeader
        , HH.slot unit Navbar.component { page: Fourth } absurd
        , HH.div
          [ cls $ container <> px0 ]
          [ HH.img 
            [ HP.src $ "http://www.scix.vip/yzmall-server/account/mine/invitation.jpg"
            , cls w100  
            ] 
          ]
        , renderFooter
        , forMobileMenu
        , mobileMenu Fourth
        , renderLoginModal
        , renderRegisterModal
        ]
      