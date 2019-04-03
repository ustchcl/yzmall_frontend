module Yzmall.Page.TradeCenter where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (guardAccount, maybeElemArray)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array (replicate, sortBy)
import Data.Lens (preview)
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe as Maybe
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Formless (initial)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import String (bgCommodity, sharePng)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount, getAccountInfo)
import Yzmall.Api.Capablity.Resource.CommodityOrder (class ManageOrder, createACTSellCommission, createACTSellRebate, createMYTSellRush, getACTSells, mytSharedRecord)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Data.ACTSell (ACTSell, MYTSharedRecord)
import Yzmall.Data.Account (Account)
import Yzmall.Data.PreciseDateTime (toDisplayTime)
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Part.Navbar as Navbar
import Yzmall.Page.Utils (alertMsg, existWhenZero, getCommodityById, getInputValue, lightYellowColor, mobileOnly, pcOnly, yellowColor)
import Yzmall.Resource.Commodity (class ManageCommodity)
import Yzmall.Utils (CardInfo, cls, (<+>), foreach_, renderBanner, renderFooter, renderHeader, renderNavBar, style, (->>))

type State =
  { currentAccount :: Maybe Account
  , sharedRecord :: RemoteData String MYTSharedRecord
  , mytSells :: RemoteData String (Array ACTSell)
  }

data Query a = 
    Initialize a
    | SellRebateMYT a
    | SellCommissionMYT a
    | GetMytSells a
    | GetMytSharedRecord a

type ChildQuery = Navbar.Query
type ChildSlot = Unit

component 
  :: forall m r
   . MonadAff m 
   => MonadAsk { currentAccount :: Ref (Maybe Account) | r} m
   => Navigate m
   => ManageCommodity m
   => ManageOrder m
   => ManageAccount m
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
    { currentAccount : Nothing
    , sharedRecord : NotAsked
    , mytSells : NotAsked
    }

  eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
  eval = case _ of
    Initialize a -> do
      guardAccount >>= case _ of 
        Nothing -> pure unit
        (Just _) -> do
          account <- getAccountInfo
          H.modify_ _ { currentAccount = account }
          void $ H.fork $ eval $ GetMytSells a
          void $ H.fork $ eval $ GetMytSharedRecord a
          pure unit
      pure a
    SellRebateMYT a -> do
      amount <- liftEffect $ getInputValue "rebate_myt_input"
      actSell <- createACTSellRebate amount
      when (isJust actSell) do
        liftEffect $ alertMsg "创建MYT出售单成功"
      account <- getAccountInfo
      H.modify_ _ {currentAccount = account}
      { currentAccount} <- ask
      liftEffect $ Ref.write account currentAccount
      void $ H.fork $ eval $ GetMytSharedRecord a
      pure a
    SellCommissionMYT a -> do 
      -- amount <- liftEffect $ getInputValue "commission_myt_input"
      actSell <- createMYTSellRush
      when (isJust actSell) do
        liftEffect $ alertMsg "创建MYT出售单成功"
      account <- getAccountInfo
      H.modify_ _ {currentAccount = account}
      { currentAccount} <- ask
      liftEffect $ Ref.write account currentAccount
      void $ H.fork $ eval $ GetMytSharedRecord a
      void $ H.fork $ eval $ GetMytSells a 
      pure a
    GetMytSells a -> do
      actSells <- getACTSells
      H.modify_ _ {mytSells = fromMaybe actSells}
      pure a
    GetMytSharedRecord a -> do
      sharedRecord' <- mytSharedRecord
      H.modify_ _ {sharedRecord = fromMaybe sharedRecord'}
      pure a
  
  render :: State -> H.ParentHTML Query ChildQuery Unit m
  render state = 
      HH.div
        [ cls $ containerFluid <> px0 
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"
        ]
        [ renderHeader
        , HH.slot unit Navbar.component { page: Third } absurd
        , HH.div
          [ cls $ container <> px0 ]
          [ HH.nav_
            [ renderNavTab
            , renderTabContent state
            ]
          ]
        , renderFooter
        , forMobileMenu
        , mobileMenu Third
        ]

    where
    mbMytSells = (sortBy (\a b -> if b.id - a.id > 0  then GT else LT)) <$> (preview _Success state.mytSells)
    mbSharedRecord = preview _Success state.sharedRecord
    price = Maybe.fromMaybe 0.0 $ _.price <$> mbSharedRecord
    remainPurchase = Maybe.fromMaybe 0.0 $ _.remainPurchase <$> mbSharedRecord
    currentPeriod = Maybe.fromMaybe 1 $ _.currentPeriod <$> mbSharedRecord
    myRebateAmount = Maybe.fromMaybe 0.0 $ _.rebateBalance <$> state.currentAccount
    myCommissionAmount = Maybe.fromMaybe 0.0 $ _.commissionBalance <$> state.currentAccount

    renderNavTab = 
      HH.div
      [ cls $ nav <> navTabs <> bgTransparent
      , "id" ->> "nav-tab"
      , "role" ->> "tablist"
      ]
      [ renderItem 0 "ds_act" "售出MYT"
      -- , renderItem 1 "fl_act" "售出返利MYT"
      , renderItem 2 "act_records" "我的MYT出售单"
      ]
    
    renderItem n id name = 
      HH.a
      [ cls $ navItem <> navLink <> bgTransparent <> textWhite <> (existWhenZero n active) <> px2 <+> "px-md-3"
      , "id" ->> "nav-" <> id <> "-tab"
      , "data-toggle" ->> "tab"
      , "href" ->> "#" <> id
      , "role" ->> "tab"
      ]
      [ HH.text name]

    renderTabContent state' = 
      HH.div 
      [ cls tabContent
      , "id" ->> "nav-tabContent"
      , style "min-height: 600px"
      ]
      [ renderTabPane "ds_act" 0 $ 
        [ HH.div
          [ cls $ w100 <> px3 <> dFlex <> flexColumn <> py3 <> mb3
          , style "border-bottom: #929292 1px solid;"
          ]
          [ HH.div
            [ cls $ w100 <> dFlex <> mb3
            , style lightYellowColor ]
            [ HH.div 
              [ cls mr3 ]
              [ HH.text "MYT市价" ]
            , HH.div_ [ HH.text $ "￥" <> show price ]
            ]
          , HH.div
            [ style lightYellowColor
            , cls $ w100 <> dFlex <> mb3
            ]
            [ HH.div 
              [ cls mr3 ]
              [ HH.text "本期抢售期数" ]
            , HH.div_ [ HH.text $ show currentPeriod ]
            ]
          , HH.div
            [ style lightYellowColor
            , cls $ w100 <> dFlex <> mb3 ]
            [ HH.div 
              [ cls mr3 ]
              [ HH.text "我拥有的MYT" ]
            , HH.div_ [ HH.text $ show $ myCommissionAmount + myRebateAmount]
            ]
          , HH.div
            [ style lightYellowColor
            , cls $ w100 <> dFlex ]
            [ HH.div 
              [ cls mr3 ]
              [ HH.text "出售数量" ]
            , HH.div_ [ HH.text $ "5000" ]
            ]
          -- , HH.div
          --   [ cls $ formInline
          --   , style lightYellowColor 
          --   ]
          --   [ HH.text "出售数量"
          --   , HH.input
          --     [ "type" ->> "text"
          --     , cls $ formControl <> ml4 <> bgTransparent <> p0
          --     , "id" ->> "commission_myt_input"
          --     , style $ "border-color: #929292; " <> lightYellowColor <> "width: 100px"
          --     , HP.disabled true
          --     , "value" ->> "5000"
          --     ]
          --   ]
          ]
        , HH.div 
          [ cls $ w100 <> dFlex]
          [ HH.div
            [ cls $ w50 <> dFlex <> flexColumn <> textWhite ]
            [ HH.text ""
            ]
          , HH.button 
            [ cls $ btn <> btnDanger <> mlAuto <> rounded0 <> dFlex <> px5
            , style "font-size: 22px; height:47px"
            , HE.onClick $ HE.input_ SellCommissionMYT
            ]
            [ HH.div
              [ cls mr4 ]
              [ HH.text "确"]
            , HH.text "定"
            ]
          ]
        ]
      , renderTabPane "act_records" 1 $ 
        [ HH.div
          [ cls $ pcOnly <> container <> mt2 <> px0
          , style "min-height: 633px"]
          [ HH.table
            [ cls $ table <> tableStriped <> tableDark <> bgTransparent]
            ([ HH.tr_
              [ HH.th
                [ "scope" ->> "col" ]
                [ HH.text "订单编号" ]
              , HH.th 
                [ "scope" ->> "col" ]
                [ HH.text "创建时间" ]
              , HH.th 
                [ "scope" ->> "col" ]
                [ HH.text "商品"]
              , HH.th 
                [ "scope" ->> "col" ]
                [ HH.text "金额" ]
              , HH.th
                [ "scope" ->> "col" ]
                [ HH.text "状态" ]
              ]
            ] <> 
            maybeElemArray mbMytSells (\mytSells -> renderPCRecord <$> mytSells))
          ]
          -- | show on mobile
        , HH.div
          [cls $ container <> mobileOnly <> flexColumn <> mxAuto <> mt2 ]
          (maybeElemArray mbMytSells (\mytSells -> renderMobileRecord <$> mytSells))
        ]
      ]
    
    

    renderTabPane id n content = 
      HH.div 
      [ cls $ tabPane <> fade <> (existWhenZero n $ active <+> "show")
      , "id" ->> id 
      , "role" ->> "tabpanel"
      ]
      content

     
     
    renderMobileRecord record' = 
        HH.div
        [ style "height: 65px; background-color: #454545"
        , cls $ dBlock <> textWhite <> mb3
        ]
        [ HH.div 
          [ cls $ dFlex <> w100 <> px2 <> mb1 <> py1 <> borderBottom
          , style "border-bottom-color: #555555!important; font-size: 15px" ]
          [ HH.text $ "订单编号 " <> show record'.id
          , HH.div
            [ cls ml2] 
            [ HH.text $ toDisplayTime record'.createTime]
          , HH.div
            [ cls mlAuto ]
            [ HH.text $ show record'.process ]
          ]
        , HH.div 
          [ cls $ dFlex <> w100 <> px2 ]
          [ HH.div
            [ style "color: #f8f2d8"
            , cls mr4
            ]
            [ HH.text $ "￥" <> show record'.price ]
          , HH.text $ "MYT" <> " * " <> show record'.amount
          ]

        ]
    renderPCRecord record' = 
        HH.tr
        [ style "border-top-color: #454545!important; border-bottom-color: #454545!important"] 
        [ HH.th
          [ "scope" ->> "row" ]
          [ HH.text $ show record'.id]
        , HH.td_ 
          [ HH.text $ toDisplayTime record'.createTime ]
        , HH.td
          [ style "color: #f8f2d8"]
          [ HH.text $ "MYT" <> " * " <> show record'.amount]
        , HH.td
          [ style yellowColor ]
          [ HH.text $ "￥" <> show record'.price ]
        , HH.td_
          [ HH.text $ show record'.process ]
        ]
