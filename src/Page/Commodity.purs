module Yzmall.Page.Commodity where

import Halogen.Themes.Bootstrap4
import Prelude

import Conduit.Component.Utils (safeHref)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import String (banner1, banner2, bgBanner, bgCommodity, bgTuiGuang, cardContent, iconWdds, iconWdfx, iconWdtg, imgUrl, rectPng)
import Yzmall.Data.Avatar (Avatar, parse)
import Yzmall.Data.Route (Route(..))
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Utils (CardInfo, cls, foreach_, renderBanner, renderCommodity, renderFooter, renderHeader, renderNavBar, style, (->>))

type CM =
    { img :: Maybe Avatar
    , name :: String
    , description :: String
    }

cm :: CM
cm =
    { img : (parse "")
    , name: "test"
    , description: "This is a description from commodity."
    }

type State = Array CM

data Query a =
    InitialCommodities a

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
        [ cm, cm ]

    render :: State -> H.ComponentHTML Query
    render state =
        HH.div
        [ cls $ containerFluid <> px0
        , style $ "overflow-x: hidden; background-image: url(" <> bgCommodity <> ")"]
        [ renderHeader
        , renderNavBar
        , HH.div
          [ cls $ w100 <> py2
          , style $ "background-image: url(" <> bgBanner <> ")" ]
          [ HH.div
            [ cls $ container <> px0 ]
            [ renderBanner [banner1, banner2] ]
          ]
        , renderMenu
        , HH.div
          [ cls $ bgTransparent <> w100]
          [ HH.div
            [ cls $ container <> px0 ]
            [ HH.div
              [ cls $ dFlex <> flexWrap ]
              [ renderCommodity 1
              , renderCommodity 2
              , renderCommodity 3
              , renderCommodity 4
              , renderCommodity 5
              ]
              -- [ renderCard cardInitialState
              -- , renderCard cardInitialState
              -- , renderCard cardInitialState ]
            ]
          ]
        , renderFooter
        , forMobileMenu
        , mobileMenu
        , renderLoginModal
        , renderRegisterModal
        ]

        where
        cardInitialState :: CardInfo
        cardInitialState =
            { title: "霸王翡翠"
            , content: cardContent
            , imgSrc: imgUrl
            , btnName: "Go somewhere"
            }

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of
        InitialCommodities next ->
        -- 1. 加载正价或特价商品
            pure next

renderMenu :: forall p i. H.HTML p i
renderMenu =
    HH.div
    [ cls $ w100
    , style $ "background-image: url(" <> bgTuiGuang <> "); background-position: top center;"
    ]
    [ HH.div
      [ cls container
      , "style" ->> "height: 120px"
      ]
      [ HH.div
        [ cls $ row <> alignItemsCenter <> h100 ]
        [ _renderImg 1 iconWdfx WDFX_ROUTE
        , _renderImg 2 iconWdtg WDTG_ROUTE
        , _renderImg 3 iconWdds WDDS_ROUTE
        ]
      ]
    ]
    where
    _renderImg n imgSrc route =
      let bordern = if (n == 1) then borderRight <> borderLeft else borderRight
      in
        HH.a
        [ cls $ col4 <> bordern <> mt2
        , safeHref route
        , style $ "height: 100px; background-image: url(" <> imgSrc <> ");background-repeat: no-repeat; background-size: contain ; background-position: center center"
        ]
        []
