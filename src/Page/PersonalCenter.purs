module Yzmall.Page.PersonalCenter where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (guardAccount, safeHref)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either.Nested (Either3)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, isNothing)
import Data.Number.Format (fixed, toStringWith)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import String (accountIcon, bgCommodity, icon_address, icon_back, icon_gold, icon_money, icon_qrcode, icon_record, icon_sale, icon_share, icon_wallet)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount, getAccountInfo, logout, setName)
import Yzmall.Api.Capablity.Resource.Address (class ManageAddress)
import Yzmall.Api.Capablity.Resource.CommodityOrder (class ManageOrder)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Route (Route(..))
import Yzmall.Data.Route2 (Route2(..))
import Yzmall.Data.Route2 as R2
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Part.Navbar as Navbar
import Yzmall.Page.Utils (BarInfo, alertMsg, darkRedColor, getInputValue, lightYellowColor, renderBar)
import Yzmall.Resource.Commodity (class ManageCommodity)
import Yzmall.Utils (cls, renderFooter, renderHeader, renderNavBar, style, (->>), (<+>))

data AddBtnState 
  = Uploading 
  | OJBK
derive instance eqAddBtnState :: Eq AddBtnState

type State = {
  account :: Maybe Account
  , addBtnState:: AddBtnState
}

type ItemConfig = Tuple BarInfo (Maybe Route)

data Query a
  = Initialize a
  | BindIDCard a
  | Logout a

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
        { account: Nothing , addBtnState: OJBK}
      eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
      eval = case _ of
        Initialize a -> do
          guardAccount >>= case _ of
            Nothing -> pure unit
            _ -> do
              account <- getAccountInfo
              H.modify_ _ { account = account }
          pure a
        BindIDCard a -> do
          name <- liftEffect $ getInputValue "idcard_name"
          id <- liftEffect $ getInputValue "idcard_id"
          H.modify_ _ {addBtnState = Uploading}
          account <- setName name id 
          H.modify_ _ { account = account }
          H.modify_ _ {addBtnState = OJBK}
          case account of 
            (Just _) -> do
              liftEffect $ alertMsg "身份认证成功"
              {currentAccount} <- ask
              liftEffect $ Ref.write account currentAccount
            Nothing -> 
              pure unit
          pure a
        Logout a -> do
          _ <- logout
          pure a

      render :: State -> H.ParentHTML Query ChildQuery Unit  m
      render { account: Nothing, addBtnState} = HH.div_ []
      render { account: Just acc, addBtnState} = 
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
          ([
          renderAccountItem acc (Just $ PC_ROUTER (Just AccountInfo))

          ] 
          <> renderAccount (if isNothing acc.idCard then Nothing else Just acc)
          <> [
            HH.a
            [ cls $ btn <> listGroupItem <> dFlex <> alignItemsCenter <> px3 <> rounded0 <> mt3 <> listGroupItemDark <> listGroupItemAction <> textWhite
            , style "font-size: 15px"
            , HE.onClick $ HE.input_ Logout
            ]
            [ HH.div
              [cls $ mxAuto <> myAuto
              , style darkRedColor
              ]
              [ HH.text "退出登录"]
            ]
          ])
        , renderFooter
        , mobileMenu Fourth
        , forMobileMenu
        ]
        where 
        renderAccount Nothing = 
          [
            HH.a
            [ cls $ listGroupItem <> dFlex <> alignItemsCenter <> px3 <> rounded0 <> listGroupItemDark <> listGroupItemAction <> textWhite
            , HP.href "#collapseBindIDCard"
            , "data-toggle" ->> "collapse"
            , style "font-size: 15px"
            ]
            [ HH.div
              [ cls $ dFlex <> flexRow <> alignItemsCenter]
              [HH.div [style darkRedColor] [ HH.text "[验证身份证激活账号]"]]
            , HH.div
              [ cls mlAuto]
              [ HH.i
                [ cls $ ml2 <+> "fas fa-chevron-right" ]
                [ ]  
              ]
            ]
          , HH.div 
            [ cls collapse 
            , "id" ->> "collapseBindIDCard"
            , style "font-size: 15px; margin-bottom: 1.5px"
            ]
            [ renderBarInput "身份证姓名" "idcard_name"
            , renderBarInput "身份证号码" "idcard_id"
            , HH.button
              [ cls $ listGroupItem <> dFlex <> alignItemsCenter <> px3 <> rounded0 <> mb3 <> listGroupItemDark <> listGroupItemAction <> textWhite
              , style "font-size: 15px"
              , HE.onClick $ HE.input_ BindIDCard
              , HP.disabled $ addBtnState == Uploading
              ]
              [ HH.div
                [cls $ mxAuto <> myAuto
                , style darkRedColor ]
                ( case addBtnState of 
                    OJBK -> [ HH.text "绑定"]
                    Uploading -> 
                      [ HH.span 
                        [ cls $ H.ClassName "spinner-border spinner-border-sm" 
                        , "role" ->> "status"
                        ]
                        []
                      , HH.text "绑定中..."
                      ]
                )
              ]
            ]

          ]
        renderAccount (Just account) = 
          (renderListGroup <$> (itemNames (account.commissionBalance + account.rebateBalance) account.gold))


      itemNames :: Number -> Number -> Array (Array ItemConfig)
      itemNames balance gold = 
        [ 
          [ Tuple (barInfo "填写邮寄地址" Nothing (Just icon_address) true) (Just $ PC_ROUTER (Just R2.AddressEditor))
          , Tuple (barInfo "绑定银行卡" Nothing (Just icon_wallet) true) (Just $ PC_ROUTER (Just R2.BankCardEditor)) 
          ]
        , [ Tuple (barInfo "MYT余额" (Just $ toStringWith (fixed 2) balance) (Just icon_money) false) Nothing 
          -- , Tuple (barInfo "金币数" (Just $ toStringWith (fixed 2) gold) (Just icon_gold) false) Nothing
          ] 
          -- <> (if true then 
          --   [ Tuple (barInfo "我要提取" (Just "未到提取时间") (Just icon_wallet) true) Nothing
          --   , Tuple (barInfo "提取记录" Nothing (Just icon_record) true) Nothing ] 
          --   else [])
        , [ Tuple (barInfo "我的推广码" (Just "(生成后长按图片保存到手机)") (Just icon_qrcode) true) (Just WDFX_ROUTE)
          , Tuple (barInfo "我的推广" Nothing (Just icon_share) true) (Just WDTG_ROUTE)
          ]
        , [ Tuple (barInfo "我的订单" Nothing (Just icon_back) true) (Just $ PC_ROUTER (Just R2.MyOrders))
          -- , Tuple (barInfo "我的代售" Nothing (Just icon_sale) true) (Just WDDS_ROUTE)
          ]
        -- , [ Tuple (barInfo "系统公告" Nothing Nothing true) Nothing
        --   , Tuple (barInfo "消息通知" Nothing Nothing true) Nothing
        --   , Tuple (barInfo "问题反馈" Nothing Nothing true) Nothing
        --   ]
        -- , [ Tuple "关于我们" $ Just "1.0.10"
        --   , Tuple "退换货政策" Nothing
        --   , Tuple "购物流程" Nothing
        --   , Tuple "保定祥琴网络科技有限公司® 版权所有" Nothing
        --   ]
        ]
      
barInfo :: String -> Maybe String -> Maybe String -> Boolean -> BarInfo
barInfo name rightDesc leftIcon withRightIcon = 
  { name
  , rightDesc
  , leftIcon
  , withRightIcon
  }
      


renderListGroup :: forall p i. Array ItemConfig -> H.HTML p i
renderListGroup arr =
  HH.div
  [ cls $ listGroup <> mb3 <> mxAuto ]
  ((\(Tuple a b) -> HH.div [ style "margin-bottom: 1px" ] [ renderBar a b ]) <$> arr)


renderAccountItem :: forall p i. Account -> Maybe Route -> H.HTML p i
renderAccountItem  account route =
  HH.a
  [ cls $ listGroupItem <> dFlex <> alignItemsCenter <> px3 <> py2<> rounded0 <> listGroupItemDark <> listGroupItemAction <> textWhite <> mb3
  , (if isJust route then safeHref $ unsafePartial $ fromJust route else "nothing" ->> "nothing")
  , style "font-size: 15px"
  ]
  [ HH.div
    [ style $ "background-image: url(" <> "./image/default.png" <> "); background-position: center center; background-size: contain; background-repeat: no-repeat; " <> "width: 70px; height: 58px" ]
    []
  , HH.div 
      [ cls $ dFlex <> flexColumn <> justifyContentBetween <> textWhite <> w100 <> pl2
        , style "height: 58px"]
      [ HH.div_ [ HH.text $ account.nickname ]
      , HH.div_ [ HH.text $ grade account.grade]
      ]
  , renderRight Nothing
  ]
  where
  grade :: Int -> String 
  grade 1 = "VIP会员"
  grade 2 = "总监"
  grade 3 = "总裁"
  grade _ = "用户"

  rightIcon true = 
    HH.i
    [ cls $ ml2 <+> "fas fa-chevron-right" ]
    []
  rightIcon false =  HH.text ""

  renderRight str =
    case str of
      Just v ->
        HH.div
        [ cls mlAuto]
        [ HH.text v
        , rightIcon true
        ]
      Nothing ->
      HH.div
        [ cls mlAuto]
        [ rightIcon true ]


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
        [ cls $ col4 <> colMd3 <> mrAuto ]
        [ HH.text name ]
      , HH.input
        [ "type" ->> "text"
        , "id" ->> id
        , cls $ formControl <> bgTransparent <> p0 <> mlAuto <> col8 <> colMd4
        , style $ "border-color: #929292; " <> lightYellowColor  <> "; height: 30px;"
        ]
      ]
    ]