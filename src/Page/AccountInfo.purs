module Yzmall.Page.AccountInfo where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (guardAccount, maybeElem, maybeElemArray, safeHref)
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.State (state)
import Data.Either (Either(..))
import Data.Lens (preview)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import Partial.Unsafe (unsafePartial)
import String (accountIcon, bgCommodity, icon_address, icon_back, icon_gold, icon_money, icon_qrcode, icon_record, icon_sale, icon_share, icon_wallet)
import Web.HTML.HTMLInputElement (name)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount, getAccountInfo)
import Yzmall.Api.Capablity.Resource.Address (class ManageAddress, AddAddressParams, addAddress, deleteAddress, myAddresses)
import Yzmall.Api.Capablity.Resource.CommodityOrder (class ManageOrder)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Address (Address)
import Yzmall.Data.Route (Route)
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu, renderBackBtn)
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Part.Navbar as Navbar
import Yzmall.Page.Utils (BarInfo, alertMsg, darkRedColor, getInputValue, getRadioBtnChecked, lightBlueColor, lightYellowColor, renderBar, unsafeAlert)
import Yzmall.Resource.Commodity (class ManageCommodity)
import Yzmall.Utils (cls, renderFooter, renderHeader, renderNavBar, style, unsafeSlug, (->>), (<+>))

type State = 
  { currentAccount :: Maybe Account
  }

data Query a
  = Initialize a

type ChildQuery = Navbar.Query
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
        { currentAccount: Nothing
        }
      eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
      eval = case _ of
        Initialize a -> do
          guardAccount >>= case _ of
            Nothing -> pure unit
            account -> do
              H.modify_ _ {currentAccount = account}
          pure a
        

      render :: State -> H.ParentHTML Query ChildQuery Unit  m
      render state =
        HH.div
        [ cls $ containerFluid <> px0
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"
        ]
        [ renderHeader
        , HH.slot unit Navbar.component { page: Fourth } absurd
        -- | show pc
        , HH.div
          [ cls $ container <> px0 <> mxAuto <> mt3
          , style "min-height: 633px"
          ]
          [ HH.div
            [ cls $ dFlex <> alignItemsCenter <> px3 <> rounded0 <> textWhite
            , style "font-size: 15px; margin-bottom: 1px; background-color: #625C4E"
            ]
            [ HH.div
              [ cls $ dFlex <> flexRow <> alignItemsCenter <> w100 <> alignItemsCenter <> py2 ]
              [ HH.div_ [ HH.text "基本信息" ] ]
            ]
          , maybeElem state.currentAccount renderBaseInfo
          ]
        , renderFooter
        , mobileMenu Fourth
        , forMobileMenu
        -- , renderBackBtn
        ]

itemClass :: H.ClassName
itemClass = listGroupItem <> dFlex <> alignItemsCenter <> px3 <> rounded0 <> listGroupItemDark <> listGroupItemAction <> textWhite

renderBaseInfo account =
  HH.div
  [ cls $ listGroup <> mb3 <> mxAuto ]
  [ bar "ID" $ show account.id
  , bar "手机" account.phone
  , bar "昵称" $  account.nickname 
  , bar "真实姓名" $ Maybe.fromMaybe "" account.name
  , bar "等级" $ grade account.grade
  ]
  where
  grade :: Int -> String 
  grade 1 = "VIP会员"
  grade 2 = "总监"
  grade 3 = "总裁"
  grade _ = "用户"

  bar leftText rightText =
    HH.div
    [ cls $ itemClass
    , style "font-size: 15px; margin-bottom: 1px"
    ]
    [ HH.div
      [ cls $ dFlex <> flexRow <> alignItemsCenter <> w100 <> alignItemsCenter ]
      [ HH.div_ [ HH.text leftText ]
      , HH.div
        [ cls mlAuto ]
        [ HH.text rightText ]
      ]
    ]

