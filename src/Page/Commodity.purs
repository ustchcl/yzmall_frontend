module Yzmall.Page.Commodity where

import Halogen.Themes.Bootstrap4
import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import String (banner1, banner2, cardContent, imgUrl, rectPng)
import Yzmall.Data.Avatar (Avatar, parse)
import Yzmall.Page.Part.Login (renderLoginModal)
import Yzmall.Utils (CardInfo, cls, foreach_, renderBanner, renderCommodity, renderFooter, renderNavBar, style, (->>))

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
    AddCommodity a
    | HowMany (Array CM -> a)

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
        HH.div_
        [ renderNavBar
        , HH.div
          [ cls $ w100 <> bgPrimary <> py2]
          [ HH.div 
            [ cls $ container <> px0 ]
            [ renderBanner [banner1, banner2] ]
          ]
        , renderMenu
        , HH.div 
          [ cls $ bgDark <> w100]
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
        -- , renderMobileMenu
        , renderLoginModal
        , HH.button
          [ "type" ->> "button"
          , cls $ btn <> btnPrimary <> btnLg <> btnBlock
          , "data-toggle" ->> "modal"
          , "data-target" ->> "#loginModal"]
          [ HH.text "test"]
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
        AddCommodity next -> 
            pure next
        HowMany len -> do
            state <- H.get
            pure (len state)


renderMenu :: forall p i. H.HTML p i
renderMenu = 
    HH.div
    [ cls $ w100 <> bgSecondary ]
    [ HH.div 
      [ cls container 
      , "style" ->> "height: 120px"
      ]
      [ HH.div 
        [ cls $ row <> alignItemsCenter <> h100 ]
        [ _renderImg 1
        , _renderImg 2
        , _renderImg 3
        ]
      ]
    ]
    where
    _renderImg n = 
      let bordern = if (n == 1) then borderRight <> borderLeft else borderRight
      in
        HH.div 
        [ cls $ col4 <> bordern <> h100
        , "data-toggle" ->> "modal"
        , "data-target" ->> "#loginModal"
        ] 
        [ HH.img
          [ HP.src rectPng
          , cls $ py3 <> mh100 <> mxAuto <> dBlock
          ]
        ]
  
renderMobileMenu :: forall p i. H.HTML p i
renderMobileMenu =
  HH.div
    [ cls $ px0 <> w100 <> dBlock <> H.ClassName "d-md-none"]
    [ HH.div
      [ cls container ]
      [ HH.nav 
        [ HP.class_  $ navbar <> navbarDark <> bgDark  <> H.ClassName "fixed-bottom" ]
        [ HH.div
          [ cls $ dFlex <> justifyContentBetween ]
          (foreach_ ["正价商品", "特价商品", "交易中心", "个人中心"] renderNavItem)
        ]
      ]

    ]
  where
  renderNavItem str n =
    HH.a 
      [ cls $ navItem <> navLink <> (if n == 0 then active else H.ClassName "")
      , HP.href "#"
      ]
      [ HH.text str ]
  