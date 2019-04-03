module Yzmall.Page.Wdds where 


import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (maybeElemArray)
import Control.Monad.Reader (class MonadAsk)
import Data.Array (replicate, sortBy)
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
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Avatar (Avatar, parse)
import Yzmall.Data.Commission (Commission)
import Yzmall.Data.Commodity (Commodity, CommodityCategory(..), forkData)
import Yzmall.Data.PreciseDateTime (toDisplayTime)
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Page.Part.Navbar (NavbarPage(..))
import Yzmall.Page.Part.Navbar as Navbar
import Yzmall.Page.Utils (mobileOnly, pcOnly, yellowColor, getCommodityById)
import Yzmall.Resource.Commodity (class ManageCommodity, viewCommissions)
import Yzmall.Utils (CardInfo, cls, foreach_, renderBanner, renderCommodity, renderFooter, renderHeader, renderNavBar, style, (->>))

type State = 
  { commissions :: RemoteData String (Array Commission)
  }

data Query a
  = Initialize a
  | GetCommissions a


type ChildQuery = 
  Navbar.Query
  
type ChildSlot = Unit


component
  :: forall m r
   . MonadAff m
  => ManageAccount m
  => ManageCommodity m
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
    { commissions : NotAsked }
  
  eval :: Query ~> H.ParentDSL State Query ChildQuery Unit Void m
  eval = case _ of
    Initialize a  -> do
      void $ H.fork $ eval $ GetCommissions a
      pure a
    GetCommissions a -> do
      commissions <- viewCommissions 
      H.modify_ _ { commissions = fromMaybe commissions}
      pure a
  
  render :: State -> H.ParentHTML Query ChildQuery Unit  m
  render state = 
      HH.div
        [ cls $ containerFluid <> px0 
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"]
        [ renderHeader
        , HH.slot unit Navbar.component { page: Fourth } absurd
        -- | show pc
        , HH.div
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
            maybeElemArray mbCommissions (\cs -> renderPCRecord <$> cs))
          ]
        -- | show on mobile
        , HH.div
          [cls $ container <> mobileOnly <> flexColumn <> mxAuto <> mt2 ]
          (maybeElemArray mbCommissions (\cs -> renderMobileRecord <$> cs))
        , renderFooter
        , forMobileMenu
        , mobileMenu Fourth
        , renderLoginModal
        , renderRegisterModal
        ]
      where 
      convert "WAIT_FOR_REVIEW" = "待审核"
      convert "ON_SALE" = "出售中"
      convert "SOLD" = "已出售"
      convert "DELIVERED" = "已收货"
      convert _  = "未知状态"

      mbCommissions = sortBy (\a b -> if b.id - a.id > 0 then GT else LT) <$> preview _Success state.commissions

      renderMobileRecord record = 
        let commodity = getCommodityById record.commodityId
        in
        HH.div
        [ style " background-color: #454545"
        , cls $ dBlock <> textWhite <> mb3 <> py1
        ]
        [ HH.div 
          [ cls $ dFlex <> w100 <> px2 <> mb1 <> pb1 <> borderBottom
          , style "border-bottom-color: #555555!important; font-size: 15px" ]
          [ HH.text $ "订单编号 " <> show record.id
          , HH.div
            [ cls ml2] 
            [ HH.text $ toDisplayTime record.createTime]
          , HH.div
            [ cls mlAuto ]
            [ HH.text $ convert record.process ]
          ]
        , HH.div 
          [ cls $ dFlex <> w100 <> px2 ]
          [ 
            HH.div
            [ style "color: #f8f2d8"
            , cls mr4
            ]
            [ HH.text $ "MYT " <> show commodity.rebateMYT ]
          , 
            HH.text $ commodity.name <> " * " <> show record.amount
          ]

        ]
      renderPCRecord record = 
        let commodity = getCommodityById record.commodityId
        in
        HH.tr
        [ style "border-top-color: #454545!important; border-bottom-color: #454545!important"] 
        [ HH.th
          [ "scope" ->> "row" ]
          [ HH.text $ show record.id ]
        , HH.td_ 
          [ HH.text $ toDisplayTime record.createTime ]
        , HH.td
          [ style "color: #f8f2d8"]
          [ HH.text $ commodity.name]
        , HH.td
          [ style yellowColor ]
          [ HH.text $ "MYT " <> show commodity.rebateMYT ]
        , HH.td_
          [ HH.text $ convert record.process ]
        ]
