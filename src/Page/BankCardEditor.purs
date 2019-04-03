module Yzmall.Page.BankCardEditor where

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
import Yzmall.Api.Capablity.Resource.BankCard (class ManageBankCard, AddBankCardParams, addBankCard, deleteBankCard, myBankCards)
import Yzmall.Api.Capablity.Resource.CommodityOrder (class ManageOrder)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.BankCard (BankCard)
import Yzmall.Data.Route (Route)
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Part.Navbar as Navbar
import Yzmall.Page.Utils (BarInfo, alertMsg, darkRedColor, getInputValue, getRadioBtnChecked, lightBlueColor, lightYellowColor, renderBar, setNumOnly, showProv, unsafeAlert)
import Yzmall.Resource.Commodity (class ManageCommodity)
import Yzmall.Utils (cls, renderFooter, renderHeader, renderNavBar, style, unsafeSlug, (->>), (<+>))

data AddBtnState 
  = Uploading 
  | OJBK
derive instance eqAddBtnState :: Eq AddBtnState


type State = 
  { bankcards :: RemoteData String (Array BankCard)
  , currentAccount :: Maybe Account
  , addBtnState :: AddBtnState
  }

data Query a
  = Initialize a
  | GetBankCards a
  | AddBankCard a
  | DeleteBankCard Int a

type ChildQuery = Navbar.Query
type ChildSlot = Unit

component
  :: forall m r
   . MonadAff m
  => ManageCommodity m
  => ManageAccount m
  => ManageOrder m
  => ManageBankCard m
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
        { bankcards: NotAsked
        , currentAccount: Nothing
        , addBtnState: OJBK
        }
      eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
      eval = case _ of
        Initialize a -> do
          guardAccount >>= case _ of
            Nothing -> pure unit
            account -> do
              liftEffect $ showProv
              H.modify_ _ {currentAccount = account}
              void $ H.fork $ eval $ GetBankCards a
              liftEffect $ setNumOnly "bankcard_card_id"
          pure a
        GetBankCards a -> do
          st <- H.modify _ { bankcards = Loading }
          bankcards <- myBankCards
          H.modify_ _ { bankcards = fromMaybe bankcards }
          pure a
        AddBankCard a -> do
          st <- H.get
          name <- liftEffect $ getInputValue "bankcard_name"
          cardId <- liftEffect $ getInputValue "bankcard_card_id"
          region <- liftEffect $ getInputValue "bankcard_region"
          phone <- liftEffect $ getInputValue "bankcard_phone"
          bank <- liftEffect $ getInputValue "bankcard_bank"
          setDefault <- liftEffect $ getRadioBtnChecked "default"
          bankDetail <- liftEffect $ getInputValue "bankcard_bank_detail"
          let 
            regionAll = if bankDetail == "" then region  else region <> "-" <> bankDetail
            bankReal = if bank == pleaseSelectBank then ""  else bank
            check = addParams <$> checkEmpty "账户姓名" name 
                              <*> checkEmpty "银行卡账户" cardId 
                              <*> checkEmpty "开户行地区" regionAll 
                              <*> checkEmpty "预留手机号" phone 
                              <*> checkEmpty "开户银行" bankReal 
                              <*> Right setDefault
          case check of
            Left err -> 
              liftEffect $ alertMsg err
            Right params -> do
              H.modify_ _ {addBtnState = Uploading}
              bc <- addBankCard params
              case bc of 
                Just _ -> liftEffect $ alertMsg "银行卡绑定成功"
                Nothing -> pure unit
              H.modify_ _ {addBtnState = OJBK}
              void $ H.fork $ eval $ GetBankCards a 
              account <- getAccountInfo
              { currentAccount } <- ask
              liftEffect $ Ref.write account currentAccount
              H.modify_ _ { currentAccount = account }
          pure a
        DeleteBankCard id a -> do
          void $ deleteBankCard $ unsafeSlug (show id)
          void $ H.fork $ eval $ GetBankCards a 
          account <- getAccountInfo
          { currentAccount } <- ask
          liftEffect $ Ref.write account currentAccount
          H.modify_ _ { currentAccount = account }
          let
            sth = unsafeAlert "银行卡解绑成功"
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
          ([renderBarInput "账户姓名" "bankcard_name" 
          , renderBarInput "银行卡账户" "bankcard_card_id"
          -- , renderBarInput "开户行地区" "bankcard_region"
          , HH.div 
            [ cls $ itemClass <> py3 <> dFlex <> flexColumn
            , style "font-size: 15px; margin-bottom: 1.5px"
            ]
            [ HH.div
              [ cls $ row <> mx0 <> dFlex <> flexRow <> w100 <> alignItemsCenter ]
              [ HH.div
                [ cls $ col5 <> colMd3 <> mrAuto <> pl0]
                [ HH.text "开户银行" ]
              , HH.select
                [ "id" ->> "bankcard_bank"
                , cls $ formControl <> colMd4 <> col7
                ]
                ((\x -> HH.option_ [ HH.text x]) <$> [ 
                  pleaseSelectBank
                ,  "工商银行"
                , "中国银行"
                , "兴业银行"
                , "中信银行"
                , "上海银行"
                , "光大银行"
                , "民生银行"
                , "北京银行"
                , "平安银行"
                , "交通银行"
                , "招商银行"
                , "广发银行"
                , "建设银行"
                , "农业银行"
                , "浦发银行"
                , "邮储银行"
                , "渤海银行"
                ])
              ] 
            ]
          , HH.div 
            [ cls $ itemClass <> py3 <> dFlex <> flexColumn
            , style "font-size: 15px; margin-bottom: 1.5px"
            ]
            [ HH.div
              [ cls $ row <> mx0 <> dFlex <> flexRow <> w100 <> alignItemsCenter ]
              [ HH.div
                [ cls $ col5 <> colMd3 <> mrAuto <> pl0]
                [ HH.text "开户行地区" ]
              , HH.input
                [ "type" ->> "text"
                , "id" ->> "bankcard_region"
                , cls $ formControl <> bgTransparent <> p0 <> mlAuto <> col7 <> colMd4
                , style $ "border-color: #929292; " <> lightYellowColor  <> "; height: 30px;"
                , HP.readOnly true
                ]
              ]
            , HH.fieldset
              [ cls $ w100 <> mt2 ]
              [ HH.form
                [ cls dFlex]
                [ HH.select
                  [ "id" ->> "prov" 
                  , "onchange" ->> "showCity(this)"
                  , cls $ customSelect <> mr2
                  ]
                  [ HH.option_ [HH.text "=请选择省份="] ]
                , HH.select
                  [ "id" ->> "city" 
                  , "onchange" ->> "showCountry(this)"
                  , cls $ customSelect <> mr2
                  ]
                  [ HH.option_ [HH.text "=请选择城市="] ]
                , HH.select
                  [ "id" ->> "country" 
                  , "onchange" ->> "selecCountry(this)"
                  , cls customSelect
                  ]
                  [ HH.option_ [HH.text "=请选择县区="] ]
                ]
              ]
            ]
          , renderBarInput "开户支行(可选)" "bankcard_bank_detail"
          , renderBarInput "预留手机号" "bankcard_phone" 
          , HH.div
            [ cls $ itemClass <> py3 <> mb3
            , style "font-size: 15px; margin-bottom: 1.5px"
            ]
            [ HH.div
              [ cls $ row <> dFlex <> flexRow <> w100]
              [ HH.div
                [ cls $ col5 <> colMd4 <> mrAuto]
                [ HH.text "是否设置默认" ]
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
            , HE.onClick $ HE.input_  AddBankCard
            , style "font-size: 15px; height: 50px"
            , HP.disabled $ state.addBtnState == Uploading
            ]
            [ HH.div
              [ cls $ mxAuto <> myAuto
              , style lightBlueColor 
              ]
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
          <> (maybeElemArray mbBankCards $ map (renderBankCard mbCardId)))
        , renderFooter
        , mobileMenu Fourth
        , forMobileMenu
        ]
        where
        mbBankCards = preview _Success state.bankcards
        mbCardId = _.defaultBankCard =<< state.currentAccount

      

itemClass :: H.ClassName
itemClass = listGroupItem <> dFlex <> alignItemsCenter <> px3 <> rounded0 <> listGroupItemDark <> listGroupItemAction <> textWhite

renderBarInput :: forall p i. String -> String -> H.HTML p i
renderBarInput name id =
  HH.div
    [ cls $ itemClass <> py3
    , style "font-size: 15px; margin-bottom: 1.5px"
    ]
    [ HH.div
      [ cls $ row <> dFlex <> flexRow <> w100 <> alignItemsCenter <> mx0]
      [ HH.div
        [ cls $ col5 <> colMd3 <> mrAuto <> pl0]
        [ HH.text name ]
      , HH.input
        [ "type" ->> "text"
        , "id" ->> id
        , cls $ formControl <> bgTransparent <> p0 <> mlAuto <> col7 <> colMd4
        , style $ "border-color: #929292; " <> lightYellowColor  <> "; height: 30px;"
        ]
      ]
    ]


renderBankCard mbCardId card =
  HH.div
  [ cls $ listGroup <> mb3 <> mxAuto ]
  [ bar "是否默认地址" (if Just true == (isDefault =<< mbCardId) then "默认" else "非默认") "#B6F9FF"
  , bar "账户姓名" card.name "#ffffff"
  , bar "银行卡账户" card.cardId "#ffffff"
  , bar "开户行地区" card.region "#ffffff"
  , bar "预留手机号" card.phone "#ffffff"
  , bar "开户银行" card.bank "#ffffff"
  -- , HH.div
  --   [ cls $ itemClass <> btn
  --   , style "font-size: 15px; margin-bottom: 1px"
  --   , HE.onClick $ HE.input_ $ DeleteBankCard card.id
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
  isDefault id = if id == card.id then Just true else Nothing

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

addParams :: String -> String -> String -> String -> String -> Boolean -> AddBankCardParams
addParams name cardId region phone bank setDefault = 
  { name
  , cardId
  , region
  , phone
  , bank
  , setDefault
  }

pleaseSelectBank :: String 
pleaseSelectBank = "=请选择开户银行="