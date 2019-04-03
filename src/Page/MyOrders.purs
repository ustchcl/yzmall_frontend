module Yzmall.Page.MyOrders where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (guardAccount, maybeElem)
import Control.Monad.Reader (class MonadAsk)
import Data.Array (filter, replicate, sortBy)
import Data.Lens (preview)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromMaybe)
import String (bgCommodity, sharePng)
import Yzmalal.Data.CommodityOrder (CommodityOrder, OrderStatus(..))
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount)
import Yzmall.Api.Capablity.Resource.CommodityOrder (class ManageOrder, deleteOrder, viewOrder)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.PreciseDateTime (toDisplayTime)
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Part.Navbar as Navbar
import Yzmall.Page.Utils (existWhenZero, getCommodityById, lightYellowColor, mobileOnly, pcOnly, yellowColor)
import Yzmall.Resource.Commodity (class ManageCommodity)
import Yzmall.Utils (CardInfo, cls, foreach_, renderBanner, renderFooter, renderHeader, renderNavBar, style, unsafeSlug, (->>), (<+>))


type State = 
  { orders :: RemoteData String (Array CommodityOrder)
  }

data Query a 
  = Initialize a
  | GetOrders a
  | DeleteOrder Int a
  -- | DeleteOrder a 

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
    { orders : NotAsked }

  eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
  eval = case _ of
    Initialize a -> do
      guardAccount >>= case _ of
            Nothing -> pure unit
            account -> do
              void $ H.fork $ eval $ GetOrders a
      pure a
    GetOrders a -> do
      orders <- viewOrder
      H.modify_ _ { orders = fromMaybe orders }
      pure a
    DeleteOrder id a -> do 
      _ <- deleteOrder $ unsafeSlug $ show id
      void $ H.fork $ eval $ GetOrders a 
      pure a
  
  render :: State -> H.ParentHTML Query ChildQuery Unit m
  render state = 
      HH.div
        [ cls $ containerFluid <> px0 
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"
        ]
        [ renderHeader
        , HH.slot unit Navbar.component { page: Fourth } absurd
        , HH.div
          [ cls $ container <> px0 ]
          [ HH.nav_
            [ renderNavTab
            , maybeElem  mbOrders renderTabContent
            ]
          ]
        , renderFooter
        , forMobileMenu 
        , mobileMenu Fourth
        ]

    where
    mbOrders = sortBy (\a b -> if b.id - a.id > 0 then GT else LT) <$> (preview _Success state.orders)

    status :: OrderStatus -> CommodityOrder -> Boolean
    status process order = 
      order.process == process
    

    renderNavTab = 
      HH.div
      [ cls $ nav <> navTabs <> bgTransparent
      , "id" ->> "nav-tab"
      , "role" ->> "tablist"
      ]
      [ renderItem 0 "all" "全部"
      , renderItem 1 "wait_for_deliver" "待发货"
      , renderItem 2 "wait_for_got" "待收货"
      , renderItem 3 "complete" "已完成"
      ]
    
    renderItem n id name = 
      HH.a
      [ cls $ navItem <> navLink <> bgTransparent <> textWhite <> (existWhenZero n active)
      , "id" ->> "nav-" <> id <> "-tab"
      , "data-toggle" ->> "tab"
      , "href" ->> "#" <> id
      , "role" ->> "tab"
      ]
      [ HH.text name]

    renderTabContent orders = 
      HH.div 
      [ cls tabContent
      , "id" ->> "nav-tabContent"
      , style "min-height: 600px"
      ]
      [ renderTabPane "all" 0 orders
      , renderTabPane "wait_for_deliver" 1  (filter (status WaitForDeliver) orders)
      , renderTabPane "wait_for_got" 2 (filter (status Delivered) orders)
      , renderTabPane "complete" 3 (filter (status Refunded) orders)
      ]
    

    renderTabPane id n records = 
      HH.div 
      [ cls $ tabPane <> fade <> (existWhenZero n $ active <+> "show")
      , "id" ->> id 
      , "role" ->> "tabpanel"
      ]
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
            (renderPCRecord <$> records))
          ]
          -- | show on mobile
        , HH.div
          [cls $ container <> mobileOnly <> flexColumn <> mxAuto <> mt2 ]
          (renderMobileRecord <$> records)
        ]

     
     
    renderMobileRecord order =
      let commodity = getCommodityById order.commodityId
      in 
        HH.div
        [ style "background-color: #454545"
        , cls $ dBlock <> textWhite <> mb3 <> py1
        ]
        [ HH.div 
          [ cls $ dFlex <> w100 <> px2 <> mb1 <> pb1 <> borderBottom
          , style "border-bottom-color: #555555!important; font-size: 15px" ]
          [ HH.text $ "订单编号 " <> show order.id
          , HH.div
            [ cls ml2] 
            [ HH.text $ toDisplayTime order.createTime]
          , HH.div
            [ cls mlAuto ]
            [ HH.text $ show order.process ]
          ]
        , HH.div 
          [ cls $ dFlex <> w100 <> px2 ]
          ([ HH.div
            [ style "color: #f8f2d8"
            , cls mr4
            ]
            [ HH.text $ "￥" <> (show $ order.payCost + order.commissionCost + order.rebateCost) ]
          , HH.text $ show commodity.name <> " * " <> show order.amount
          ] <> if order.process == WaitForPayment then [ HH.button [ cls $ btn <> btnDanger, HE.onClick $ HE.input_ $ DeleteOrder order.id] [ HH.text "删除"] ] else [])

        ]
    renderPCRecord order = 
      let commodity = getCommodityById order.commodityId
      in
        HH.tr
        [ style "border-top-color: #454545!important; border-bottom-color: #454545!important"] 
        [ HH.th
          [ "scope" ->> "row" ]
          [ HH.text $ show order.id]
        , HH.td_ 
          [ HH.text $ toDisplayTime order.createTime ]
        , HH.td
          [ style "color: #f8f2d8"]
          [ HH.text $ commodity.name]
        , HH.td
          [ style yellowColor ]
          [ HH.text $ "￥" <> (show $ order.payCost + order.commissionCost + order.rebateCost) ]
        , HH.td_
          ([ HH.text $ show order.process ] <> if order.process == WaitForPayment then [ HH.button [ cls $ btn <> btnDanger, HE.onClick $ HE.input_ $ DeleteOrder order.id] [ HH.text "删除"] ] else [])
        ]
