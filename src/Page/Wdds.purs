module Yzmall.Page.Wdds where 


import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Data.Array (replicate)
import Data.Maybe (Maybe(..))
import Formless (initial)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import String (banner1, bgCommodity, bgInfoTitle, c105Info)
import Yzmall.Data.Avatar (Avatar, parse)
import Yzmall.Data.Commodity (Commodity, CommodityCategory(..), forkData)
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Page.Utils (mobileOnly, pcOnly, yellowColor)
import Yzmall.Utils (CardInfo, cls, foreach_, renderBanner, renderCommodity, renderFooter, renderHeader, renderNavBar, style, (->>))

type Record = 
  { id :: Int
  , date :: String
  , price :: Number
  , name :: String 
  , amount :: Int
  , status :: String 
  }

type State = Array Record

data Query a = 
    InitCommodityInfo a

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
      { initialState: const initialState 
      , render
      , eval 
      , receiver: const Nothing
      }
  where
  initialState :: State
  initialState = 
    replicate 20 { id : 123
    , date: "2019-03-11 17:34:44"
    , price: 2.0
    , name: "尼泊尔小金刚尼泊尔小金刚"
    , amount: 1
    , status : "已售出"
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    InitCommodityInfo next -> 
      pure next
  
  render :: State -> H.ComponentHTML Query
  render state = 
      HH.div
        [ cls $ containerFluid <> px0 
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"]
        [ renderHeader
        , renderNavBar
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
            (renderPCRecord <$> state))
          ]
        -- | show on mobile
        , HH.div
          [cls $ container <> mobileOnly <> flexColumn <> mxAuto <> mt2 ]
          (renderMobileRecord <$> state)
        , renderFooter
        , forMobileMenu
        , mobileMenu
        , renderLoginModal
        , renderRegisterModal
        ]
      where 
      renderMobileRecord record = 
        HH.div
        [ style "height: 65px; background-color: #454545"
        , cls $ dBlock <> textWhite <> mb3
        ]
        [ HH.div 
          [ cls $ dFlex <> w100 <> px2 <> mb1 <> py1 <> borderBottom
          , style "border-bottom-color: #555555!important; font-size: 15px" ]
          [ HH.text $ "订单编号 " <> show record.id
          , HH.div
            [ cls ml2] 
            [ HH.text record.date]
          , HH.div
            [ cls mlAuto ]
            [ HH.text record.status ]
          ]
        , HH.div 
          [ cls $ dFlex <> w100 <> px2 ]
          [ HH.div
            [ style "color: #f8f2d8"
            , cls mr4
            ]
            [ HH.text $ "￥" <> show record.price ]
          , HH.text $ record.name <> " * " <> show record.amount
          ]

        ]
      renderPCRecord record = 
        HH.tr
        [ style "border-top-color: #454545!important; border-bottom-color: #454545!important"] 
        [ HH.th
          [ "scope" ->> "row" ]
          [ HH.text $ show record.id ]
        , HH.td_ 
          [ HH.text record.date ]
        , HH.td
          [ style "color: #f8f2d8"]
          [ HH.text record.name]
        , HH.td
          [ style yellowColor ]
          [ HH.text $ "￥" <> show record.price ]
        , HH.td_
          [ HH.text record.status ]
        ]
