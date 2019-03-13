module Yzmall.Page.CommodityInfo where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

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

type State = Commodity

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
  initialState = forkData

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
          [ cls $ row <> container <> mxAuto <> px0 <> mt2 <> pcOnly
          , style "height:470px"
          ]
          [ HH.div 
            [cls $ colMd8 <> col12 <> pl0 <> h100
            , style $ "background-image: url(" <> banner1 <> "); background-position: center center; background-size: cover"
            ]
            []
          , HH.div 
            [ cls $ colMd4 <> pr0 <> dFlex <> flexColumn <> h100
            ]
            [ HH.h3
              [ cls textWhite ]
              [ HH.text state.name ]
            , HH.h2 
              [ style yellowColor ]  
              [ HH.text $ "￥" <> show state.price <> "元" ]
            , HH.h5
              [ style yellowColor ]
              [ HH.text $ "("<> (if state.category == Regular then "需消耗" else "可获得") <> show state.gold <> "金币)"]
            , HH.div
              [ cls $ dFlex <> flexColumn <> textWhite <> pt2 <> mbAuto
              , style "border-top: 1px dashed #f8f2d8"
              ]
              [ HH.div 
                [ cls $ mb4 ]
                [ HH.text "运费: 包邮" ]
              , HH.div 
                [ cls mb4]
                [ HH.text $ "销量: " <> show state.sale ]
              , HH.div_
                [ HH.text $ "库存: " <> show state.stock ]
              ]
            , HH.button
              [ cls $ btn <> btnDanger <> w100 <> rounded0 <> pt0
              , style "font-size: 38px" ]
              [ HH.div 
                [ cls myAuto ]
                [ HH.text "立即购买" ]
              ]
            ]
          ]
        -- | show on mobile
        , HH.div
          [cls $ container <> px0 <> mobileOnly <> flexColumn <> mxAuto]
          [ HH.img
            [ HP.src banner1 
            , cls mw100
            ]
          , HH.div 
            [ cls $ bgDanger <> dBlock <> px1 <> mb2]
            [ HH.div 
              [ cls $ floatLeft <> textWhite <> h3 <> my1 ]
              [ HH.text $ "￥" <> show state.price <> "元"]
            , HH.div
              [ cls $ floatRight <> mt2
              , style "color: #f8f2d8"
              ]
              [ HH.text $ "（需消耗" <> show state.gold <> "金币）" ]
            ]
          , HH.div
            [ cls $  row <> container <> px0 <> mxAuto]
            [ HH.div 
              [ cls $ col7 <> px1 ]
              [ HH.h4 
                [ style "color: #f8f2d8" ]
                [ HH.text $ state.name ]
              , HH.div
                [ cls $ w100 <> pt2 <> textWhite
                , style "border-top: 1px dashed #f8f2d8"
                ]
                [ HH.div 
                  [ cls $ mb1 ]
                  [ HH.text "运费: 包邮" ]
                , HH.div 
                  [ cls mb1]
                  [ HH.text $ "销量: " <> show state.sale ]
                , HH.div_
                  [ HH.text $ "库存: " <> show state.stock ]
                ]
              ]
            ]
          ]
        , HH.div
          [ cls $ container <> px0 <> dFlex <> flexColumn <> h100]
          [ HH.img
            [ HP.src bgInfoTitle
            , cls $ w100 <> py2
            ]
          , HH.img 
            [ cls w100 
            , HP.src c105Info ]
          ]
        , renderFooter
        , HH.div
          [ cls mobileOnly
          , style "height: 50px"
          ]
          []
        , HH.div
          [ cls mobileOnly ]
          [ HH.button 
            [ cls $ btn <> btnDanger <> w100 <> fixedBottom
            , style "font-size: 24px"
            ]
            [ HH.text "立即购买"]
          ]
        , renderLoginModal
        , renderRegisterModal
        ]
