module Yzmall.Page.AddressEditor where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (guardAccount, maybeElem, maybeElemArray, safeHref)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (Either(..))
import Data.Lens (preview)
import Data.Maybe (Maybe(..), fromJust, isJust)
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
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Part.Navbar as Navbar
import Yzmall.Page.Utils (BarInfo, alertMsg, darkRedColor, getInputValue, getRadioBtnChecked, lightBlueColor, lightYellowColor, renderBar, unsafeAlert)
import Yzmall.Resource.Commodity (class ManageCommodity)
import Yzmall.Utils (cls, renderFooter, renderHeader, renderNavBar, style, unsafeSlug, (->>), (<+>))

type State = 
  { addresses :: RemoteData String (Array Address)
  , currentAccount :: Maybe Account
  , addBtnState :: AddBtnState
  }

data AddBtnState 
  = Uploading 
  | OJBK
derive instance eqAddBtnState :: Eq AddBtnState

data Query a
  = Initialize a
  | GetAddresses a
  | AddAddress a
  | DeleteAddress Int a

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
        { addresses: NotAsked
        , currentAccount: Nothing
        , addBtnState: OJBK
        }
      eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
      eval = case _ of
        Initialize a -> do
          guardAccount >>= case _ of
            Nothing -> pure unit
            account -> do
              H.modify_ _ {currentAccount = account}
              void $ H.fork $ eval $ GetAddresses a
          pure a
        GetAddresses a -> do
          st <- H.modify _ { addresses = Loading }
          addresses <- myAddresses
          H.modify_ _ { addresses = fromMaybe addresses }
          pure a
        AddAddress a -> do
          st <- H.get
          name <- liftEffect $ getInputValue "address_name"
          phone <- liftEffect $ getInputValue "address_phone"
          address <- liftEffect $ getInputValue "address_address"
          setDefault <- liftEffect $ getRadioBtnChecked "default"
          let 
            check = addParams <$> checkEmpty "联系人姓名" name 
                              <*> checkEmpty "地址" address 
                              <*> checkEmpty "电话" phone 
                              <*> Right setDefault
          case check of
            Left err -> 
              liftEffect $ alertMsg err
            Right params -> do
              H.modify_ _ {addBtnState = Uploading}
              addr <- addAddress params
              H.modify_ _ {addBtnState = OJBK}
              void $ H.fork $ eval $ GetAddresses a 
              account <- getAccountInfo
              { currentAccount } <- ask
              liftEffect $ Ref.write account currentAccount
              H.modify_ _ { currentAccount = account }
              pure $ unsafeAlert "地址添加成功"
          pure a
        DeleteAddress id a -> do
          void $ deleteAddress $ unsafeSlug (show id)
          void $ H.fork $ eval $ GetAddresses a 
          account <- getAccountInfo
          { currentAccount } <- ask
          liftEffect $ Ref.write account currentAccount
          H.modify_ _ { currentAccount = account }
          let
            sth = unsafeAlert "地址删除成功"
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
          ([ renderBarInput "联系人姓名" "address_name"
          , renderBarInput "地址" "address_address"
          , renderBarInput "电话" "address_phone"
          , HH.div
            [ cls $ itemClass <> py3 <> mb3
            , style "font-size: 15px; margin-bottom: 1.5px"
            ]
            [ HH.div
              [ cls $ row <> dFlex <> flexRow <> w100]
              [ HH.div
                [ cls $ col5 <> colMd4 <> mrAuto]
                [ HH.text "是否默认地址" ]
              , HH.div
                [ cls $ bgTransparent <> p0 <> mlAuto <> col7 <> colMd4 <> dFlex <> justifyContentEnd ]
                [ HH.div
                  [ cls $ formCheck <> formCheckInline  ]
                  [ HH.input
                    [ cls formCheckInput
                    , "type" ->> "radio"
                    , "name" ->> "inlineRadioOptions"
                    , "id" ->> "notDefault"
                    , "value" ->> "option1"
                    ]
                  , HH.label
                    [ cls formCheckLabel
                    , "for" ->> "notDefault"
                    ]
                    [ HH.text "不默认" ]
                  ]
                , HH.div
                  [ cls $ formCheck <> formCheckInline <> mr0 ]
                  [ HH.input
                    [ cls $ formCheckInput 
                    , "type" ->> "radio"
                    , "name" ->> "inlineRadioOptions"
                    , "id" ->> "default"
                    , "value" ->> "option2"
                    , "checked" ->> "true"
                    ]
                  , HH.label
                    [ cls formCheckLabel
                    , "for" ->> "default"
                    ]
                    [ HH.text "默认" ]
                  ]
                ]
              ]
            ]
          , HH.button
            [ cls $ itemClass <> mb3 <> btn
            , HE.onClick $ HE.input_  AddAddress
            , style "font-size: 15px; height: 50px"
            , HP.disabled $ state.addBtnState == Uploading
            ]
            [ HH.div
              [cls $ mxAuto <> myAuto
              , style lightBlueColor ]
              ( case state.addBtnState of 
                  OJBK -> [ HH.text "确认提交"]
                  Uploading -> 
                    [ HH.span 
                      [ cls $ H.ClassName "spinner-border spinner-border-sm" 
                      , "role" ->> "status"
                      ]
                      []
                    , HH.text "提交中..."
                    ]
              )
            ]
          ]
          <> (maybeElemArray mbAddresses $ map (renderAddress mbAddrId)))
        , renderFooter
        , mobileMenu Fourth
        , forMobileMenu
        ]
        where
        mbAddresses = preview _Success state.addresses
        mbAddrId = _.defaultAddress =<< state.currentAccount

      

itemClass :: H.ClassName
itemClass = listGroupItem <> dFlex <> alignItemsCenter <> px3 <> rounded0 <> listGroupItemDark <> listGroupItemAction <> textWhite

renderBarInput :: forall p i. String -> String -> H.HTML p i
renderBarInput name id =
  HH.div
    [ cls $ itemClass <> py3
    , style "font-size: 15px; margin-bottom: 1.5px"
    ]
    [ HH.div
      [ cls $ row <> dFlex <> flexRow <> w100 <> alignItemsCenter ]
      [ HH.div
        [ cls $ col5 <> colMd3 <> mrAuto ]
        [ HH.text name ]
      , HH.input
        [ "type" ->> "text"
        , "id" ->> id
        , cls $ formControl <> bgTransparent <> p0 <> mlAuto <> col7 <> colMd4
        , style $ "border-color: #929292; " <> lightYellowColor  <> "; height: 30px;"
        ]
      ]
    ]


renderAddress mbAddrId addr =
  HH.div
  [ cls $ listGroup <> mb3 <> mxAuto ]
  [ bar "是否默认地址" (if Just true == (isDefault =<< mbAddrId) then "默认" else "非默认") "#B6F9FF"
  , bar "联系人" addr.name "#ffffff"
  , bar "地址" addr.address "#ffffff"
  , bar "电话" addr.phone "#ffffff"
  -- , HH.div
  --   [ cls $ itemClass <> btn
  --   , style "font-size: 15px; margin-bottom: 1px"
  --   , HE.onClick $ HE.input_ $ DeleteAddress addr.id
  --   ]
  --   [ HH.div
  --     [ cls $ dFlex <> flexRow <> alignItemsCenter <> w100 <> alignItemsCenter ]
  --     [ HH.div_ [ HH.text "操作" ]
  --     , HH.div
  --       [ cls mlAuto
  --       , style $ "color: #F05451"
  --       ]
  --       [ HH.text "[删除]" ]
  --     ]
  --   ]
  ]
  where
  isDefault id = if id == addr.id then Just true else Nothing

  bar leftText rightText rightColor =
    HH.div
    [ cls $ itemClass
    , style "font-size: 15px; margin-bottom: 1px"
    ]
    [ HH.div
      [ cls $ dFlex <> flexRow <> alignItemsCenter <> w100 <> alignItemsCenter ]
      [ HH.div
        [ style "min-width: 32px;"] 
        [ HH.text leftText ]
      , HH.div
        [ cls mlAuto
        , style $ "color: " <> rightColor
        ]
        [ HH.text rightText ]
      ]
    ]

-- addAddress
--   :: forall m
--    . ManageAddress m
--   => m Unit
-- addAddress = do
--   name <- liftEffect $ getInputValue "address_name"
--   phone <- liftEffect $ getInputValue "address_phone"
--   address <- liftEffect $ getInputValue "address_address"

--   pure unit

checkEmpty :: String -> String -> Either String String
checkEmpty key "" = Left $ key <> "不可为空"
checkEmpty _ str = Right str

addParams :: String -> String -> String -> Boolean -> AddAddressParams
addParams name address phone setDefault = 
  { name
  , address
  , phone
  , setDefault
  }