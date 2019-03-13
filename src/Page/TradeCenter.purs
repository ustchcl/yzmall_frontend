module Yzmall.Page.TradeCenter where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Data.Array (replicate)
import Data.Maybe (Maybe(..))
import Formless (initial)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import String (bgCommodity, sharePng)
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Page.Utils (existWhenZero, lightYellowColor, mobileOnly, pcOnly, yellowColor)
import Yzmall.Utils (CardInfo, cls, (<+>), foreach_, renderBanner, renderFooter, renderHeader, renderNavBar, style, (->>))

type SaleRecord = 
  { id :: Int
  , date :: String
  , price :: Number
  , name :: String 
  , amount :: Int
  , status :: String 
  }

type ActInfo = 
  { price :: Number
  , remainRequire :: Int
  , myReturnActAmount :: Int
  , mySaleActAmount :: Int
  , records :: Array SaleRecord
  }

record :: SaleRecord
record = 
    { id : 123
    , date: "2019-03-11 17:34:44"
    , price: 2.0
    , name: "S-80 我爱你手镯网红爱的记忆钛钢18K玫瑰金手链"
    , amount: 1
    , status : "已售出"
    }


type State = ActInfo

data Query a = 
    InitWdfxInfo a

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
    { price : 1.01
    , remainRequire : 20000
    , myReturnActAmount: 100
    , mySaleActAmount: 200
    , records: (replicate 20 record)
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    InitWdfxInfo next -> 
      pure next
  
  render :: State -> H.ComponentHTML Query
  render state = 
      HH.div
        [ cls $ containerFluid <> px0 
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"
        ]
        [ renderHeader
        , renderNavBar
        , HH.div
          [ cls $ container <> px0 ]
          [ HH.nav_
            [ renderNavTab
            , renderTabContent state
            ]
          ]
        , renderFooter
        , forMobileMenu
        , mobileMenu
        , renderLoginModal
        , renderRegisterModal
        ]

    where
    renderNavTab = 
      HH.div
      [ cls $ nav <> navTabs <> bgTransparent
      , "id" ->> "nav-tab"
      , "role" ->> "tablist"
      ]
      [ renderItem 0 "ds_act" "售出 代售ACT"
      , renderItem 1 "fl_act" "售出 返利ACT"
      , renderItem 2 "act_records" "我的ACT出售单"
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
              [ HH.text "ACT市价" ]
            , HH.div_ [ HH.text $ "￥" <> show state'.price ]
            ]
          , HH.div
            [ style lightYellowColor
            , cls $ w100 <> dFlex <> mb3
            ]
            [ HH.div 
              [ cls mr3 ]
              [ HH.text "剩余求购数量" ]
            , HH.div_ [ HH.text $ show state'.mySaleActAmount ]
            ]
          , HH.div
            [ style lightYellowColor
            , cls $ w100 <> dFlex <> mb3 ]
            [ HH.div 
              [ cls mr3 ]
              [ HH.text "我拥有的代售ACT" ]
            , HH.div_ [ HH.text $ show state'.mySaleActAmount ]
            ]
          , HH.div
            [ cls $ formInline
            , style lightYellowColor 
            ]
            [ HH.text "出售数量"
            , HH.input
              [ "type" ->> "text"
              , cls $ formControl <> ml4 <> bgTransparent <> p0
              , style $ "border-color: #929292; " <> lightYellowColor <> "width: 100px"
              ]
            ]
          ]
        , HH.button 
          [ cls $ btn <> btnDanger <> floatRight <> rounded0 <> dFlex <> px5
          , style "font-size: 22px"
          ]
          [ HH.div
            [ cls mr4 ]
            [ HH.text "确"]
          , HH.text "定"
          ]
        ]
      , renderTabPane "fl_act" 1 $ 
        [ HH.div
          [ cls $ w100 <> px3 <> dFlex <> flexColumn <> py3 <> mb3
          , style "border-bottom: #929292 1px solid;" 
          ]
          [ HH.div
            [ cls $ w100 <> dFlex <> mb3
            , style lightYellowColor ]
            [ HH.div 
              [ cls mr3 ]
              [ HH.text "ACT市价" ]
            , HH.div_ [ HH.text $ "￥" <> show state'.price ]
            ]
          , HH.div
            [ style lightYellowColor
            , cls $ w100 <> dFlex <> mb3
            ]
            [ HH.div 
              [ cls mr3 ]
              [ HH.text "剩余求购数量" ]
            , HH.div_ [ HH.text $ show state'.mySaleActAmount ]
            ]
          , HH.div
            [ style lightYellowColor
            , cls $ w100 <> dFlex <> mb3 ]
            [ HH.div 
              [ cls mr3 ]
              [ HH.text "我拥有的返利ACT" ]
            , HH.div_ [ HH.text $ show state'.mySaleActAmount ]
            ]
          , HH.div
            [ cls $ formInline
            , style lightYellowColor 
            ]
            [ HH.text "出售数量"
            , HH.input
              [ "type" ->> "text"
              , cls $ formControl <> ml4 <> bgTransparent <> p0
              , style $ "border-color: #929292; " <> lightYellowColor <> "width: 100px"
              ]
            ]
          ]
        , HH.button 
          [ cls $ btn <> btnDanger <> floatRight <> rounded0 <> dFlex <> px5
          , style "font-size: 22px"
          ]
          [ HH.div
            [ cls mr4 ]
            [ HH.text "确"]
          , HH.text "定"
          ]
        ]
      , renderTabPane "act_records" 2 $ 
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
            (renderPCRecord <$> state'.records))
          ]
          -- | show on mobile
        , HH.div
          [cls $ container <> mobileOnly <> flexColumn <> mxAuto <> mt2 ]
          (renderMobileRecord <$> state'.records)
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
            [ HH.text record'.date]
          , HH.div
            [ cls mlAuto ]
            [ HH.text record'.status ]
          ]
        , HH.div 
          [ cls $ dFlex <> w100 <> px2 ]
          [ HH.div
            [ style "color: #f8f2d8"
            , cls mr4
            ]
            [ HH.text $ "￥" <> show record'.price ]
          , HH.text $ record'.name <> " * " <> show record'.amount
          ]

        ]
    renderPCRecord record' = 
        HH.tr
        [ style "border-top-color: #454545!important; border-bottom-color: #454545!important"] 
        [ HH.th
          [ "scope" ->> "row" ]
          [ HH.text $ show record'.id]
        , HH.td_ 
          [ HH.text record'.date ]
        , HH.td
          [ style "color: #f8f2d8"]
          [ HH.text record'.name]
        , HH.td
          [ style yellowColor ]
          [ HH.text $ "￥" <> show record'.price ]
        , HH.td_
          [ HH.text record'.status ]
        ]
