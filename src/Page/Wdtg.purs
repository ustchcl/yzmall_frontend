module Yzmall.Page.Wdtg where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Affjax.RequestBody (RequestBody(..))
import Conduit.Component.Utils (maybeElemArray, whenElem)
import Control.Monad.Reader (class MonadAsk)
import Data.Array (length, replicate)
import Data.Lens (preview)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Formless (initial)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import String (banner1, bgCommodity, bgInfoTitle, c105Info)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount)
import Yzmall.Api.Capablity.Resource.Account (getInvitees)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Avatar (Avatar, parse)
import Yzmall.Data.Commodity (Commodity, CommodityCategory(..), forkData)
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Part.Navbar as Navbar
import Yzmall.Page.Utils (mobileOnly, pcOnly, yellowColor)
import Yzmall.Utils (CardInfo, cls, foreach_, renderBanner, renderCommodity, renderFooter, renderHeader, renderNavBar, style, (->>))

type State = 
  { accounts :: RemoteData String (Array Account)
  }

data Query a
  = Initialize a
  | Invitees a

type ChildQuery = 
  Navbar.Query
  
type ChildSlot = Unit


component
  :: forall m r
   . MonadAff m
  => ManageAccount m
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
  initialState = 
    { accounts : NotAsked }

  eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
  eval = case _ of
    Initialize a -> do
      void $ H.fork $ eval $ Invitees a
      pure a
    Invitees a -> do
      accounts <- getInvitees
      H.modify_ _ {accounts = fromMaybe accounts}
      pure a
  
  render :: State -> H.ParentHTML Query ChildQuery Unit  m
  render state = 
      HH.div
        [ cls $ containerFluid <> px0 
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"]
        [ renderHeader
        , HH.slot unit Navbar.component { page: Fourth } absurd
        , HH.div
          [cls $ container <> flexColumn <> mxAuto <> mt2
          , style "min-height: 633px" ]
          ((maybeElemArray mbAccounts (\accounts -> renderRecord <$> accounts)) <> 
          [ whenElem ((length <$> mbAccounts) == Just 0) $ \_ -> 
              HH.div 
              [ cls textWhite ]
              [ HH.text "暂时没有任何推广"]

          ])

        , renderFooter
        , forMobileMenu
        , mobileMenu Fourth
        ]
      where 
      mbAccounts = preview _Success state.accounts

      grade :: Int -> String 
      grade 1 = "VIP会员"
      grade 2 = "总监"
      grade 3 = "总裁"
      grade _ = "用户"

      renderRecord account = 
        HH.div
        [ cls $ dBlock <> textWhite <> mb1 <> bgTransparent <> borderBottom <> px0
        , style "height: 72px; font-size: 15px; border-bottom-color: #555555!important" ]
        [ HH.div 
          [ cls $ row <> mw100 <> mx0]
          [ HH.img 
              [ cls $ colMd1 <> col3 <> myAuto <> dBlock <> px0 <> mxAuto
              , HP.src "./image/default.png"
              , style "max-width: 60px; max-height: 60px;"
              ]
            , HH.div
              [ cls $ colMd11 <> col9 <> dFlex <> flexColumn <> alignContentBetween ]
              [ HH.div_ [ HH.text account.nickname ]
              , HH.div_ [ HH.text $ grade account.grade ]
              -- , HH.div_ [ HH.text $ "推广 " <> show record.amount 
              ]
          
          ]
        ]