module Yzmall.Utils where
import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Data.Array (zip, (..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import String (imgUrl)
import Web.HTML.Event.EventTypes (cancel)
import Yzmall.Data.Commodity (Commodity)

type CardInfo =
  { title :: String
  , content :: String
  , imgSrc :: String
  , btnName :: String
}



-- | Card
renderCard :: forall p i. CardInfo -> HH.HTML p i
renderCard state =
      HH.div
        [ HP.class_ $ card <> H.ClassName "col-6" <> bgTransparent
        ]
        [ HH.img
          [ HP.class_ $  cardImgTop
          , HP.src state.imgSrc
          , HP.alt "Card image cap"
          ]
        , HH.div
          [ HP.class_ cardBody ]
          [ HH.h5
            [ HP.class_ cardTitle ]
            [ HH.text state.title ]
          , HH.p
            [ HP.class_ cardText ]
            [ HH.text state.content]
          ]
        ]

renderCommodity :: forall p i. Int -> H.HTML p i
renderCommodity n = 
  let pLeftOrRight = if n `mod` 2 == 1 then H.ClassName "pr-md-1 pl-md-0" else H.ClassName "pr-md-0 pl-md-1"
  in
  HH.div
  [ cls $ colMd6 <> card <> my2 <> border0 <> px0 <> pLeftOrRight <> bgTransparent]
  [ HH.img
    [ cls $ cardImgTop <> pLeftOrRight
    , HP.src imgUrl
    ]
  , HH.div
    [ cls $ cardBody <> row <> px0 <> py1 <> alignMiddle ]
    [ HH.div
      [ cls $ col9]
      [ HH.p
        [ cls $ cardText <> textTruncate <> w100 <> pl1
        , "style" ->> "color: #DFDBC6"
        ]
        [ HH.text "S-120 开光天然东陵玉貔貅吊坠"]
      ]
    , HH.div
      [ cls $ col3  ]
      [ HH.small
        [ "style" ->> "color: #DFDBC6"
        , cls $ floatRight <> pr1 <> pt1]
        [ HH.text "销量:100"]
      ]
    , HH.div
      [cls col6]
      [ HH.b
        [ cls $ cardText <> w100 <> pl1
        , style "font-size: 150%; color: #F6C17F" 
        ]
        [HH.text "￥2500.00"]
      ]
    , HH.div
      [cls col6]
      [ HH.b
        [ cls $ floatRight <> pr1 <> pt1
        , style "color: #F6C17F" 
        ]
        [HH.text "(赠送1000金币)"]
      ]
    ]
  ]

shadow_ :: String
shadow_ = "box-shadow: 0px 4px 18px 0 rgba(0,0,0,0.2), 0px 6px 30px 0 rgba(0,0,0,0.19)"

attr_ :: forall r i. String -> String -> IProp r i
attr_ key value = HP.attr (H.AttrName key) value

infix 4 attr_ as ->>

style :: forall r i. String -> IProp r i
style = attr_ "style"

-- | Banner
renderBanner :: forall p i. Array String -> HH.HTML p i
renderBanner state =
      HH.div
        [ HP.class_ $ mxAuto <> myAuto ]
        [
          HH.div
            [ HP.class_ $ carousel <> (H.ClassName "slide")
            , attr_ "data-ride" "carousel"
            , attr_ "id" "carouselControls"
            ]
            [ HH.ol
              [ HP.class_ carouselIndicators]
              (foreach_ (map (const "carouselControls") state) renderli)
            , HH.div
                [ HP.class_ carouselInner ]
                (foreach_ state renderSlider)
            , HH.a
              [ HP.class_ carouselControlPrev
              , HP.href "#carouselControls"
              , attr_ "role" "button"
              , attr_ "data-slide" "prev"
              ]
              [ HH.span
                [ HP.class_ carouselControlPrevIcon
                , attr_ "aria-hidder" "true"
                ]
                []
              , HH.span
                [ HP.class_ srOnly ]
                [ HH.text "Previous"]
              ]
            , HH.a
              [ HP.class_ carouselControlNext
              , HP.href "#carouselControls"
              , attr_ "role" "button"
              , attr_ "data-slide" "next"
              ]
              [ HH.span
                [ HP.class_ carouselControlNextIcon
                , attr_ "aria-hidder" "true"
                ]
                []
              , HH.span
                [ HP.class_ srOnly ]
                [ HH.text "Next"]
              ]
            ]
          ]

renderSlider :: forall p i. String -> Int -> HH.HTML p i
renderSlider src n =
    HH.div
      [ HP.class_ $ carouselItem <> (if n == 0 then active else H.ClassName "") 
      ] 
      [ HH.img
        [ HP.class_ $ dBlock <> w100
        , HP.src src
        , HP.alt $ "slider" <> show n
        ]
      ]

  -- |   <ol class="carousel-indicators">
  --  <li data-target="#carouselExampleIndicators" data-slide-to="0" class="active"></li>
  --  <li data-target="#carouselExampleIndicators" data-slide-to="1"></li>
  --  <li data-target="#carouselExampleIndicators" data-slide-to="2"></li>
  -- </ol>
renderli :: forall p i. String -> Int -> HH.HTML p i
renderli src n =
    HH.li
      [ attr_ "data-target" $ "#" <> src
      , attr_ "data-slide-to" (show n)
      , HP.class_ $ if n == 0 then active else H.ClassName ""
      ]
      []

foreach_ :: forall a b. Array a -> (a -> Int -> b) -> Array b
foreach_ arr f =
  map (\(Tuple t1 t2) -> f t2 t1) temp
  where
    temp = zip (0 .. 99) arr

-- | Navbar

append :: H.ClassName -> String -> H.ClassName
append a b = a <> (H.ClassName b)
infix 4 append as <+>

cls :: ∀ r i. H.ClassName → IProp ( "class" ∷ String | r ) i
cls = HP.class_

renderNavBar :: ∀ p i. HH.HTML p i
renderNavBar = 
  HH.nav 
  [ HP.class_  $ navbar <> H.ClassName "bd-navbar" <> navbarExpandMd <> navbarDark <> bgDark  <+> "sticky-top" ]
  [ HH.div
    [ cls $ container <> px0]
    [ HH.a 
      [ cls $ navbarBrand
      , HP.href "#"
      ]
      [ HH.text "银洲国际" ]
    , HH.button 
      [ HP.class_ navbarToggler
      , "data-toggle" ->> "collapse"
      , "data-target" ->> "#navbarNav"
      , "type" ->> "button"
      ]
      [ HH.span [cls navbarTogglerIcon ] [] ]
    , HH.div
      [ cls $ collapse <> navbarCollapse 
      , "id" ->> "navbarNav" 
      ]
      (foreach_ ["正价商品", "特价商品", "交易中心", "个人中心"] renderNavItem)
    ]
  ]
  
  where
  renderNavItem str n =
    HH.a 
      [ cls $ navItem <> navLink <> (if n == 0 then active else H.ClassName "")
      , HP.href "#"
      ]
      [ HH.text str ]

removerGutter :: String
removerGutter = "padding-left: -15px; margin-right: -15px;"


renderFooter :: ∀ p i. HH.HTML p i
renderFooter =
  HH.div 
  [ cls $ bgWarning <> dFlex <> flexColumn <> pt2 <> textDark
  , style "height: 90px"
  ]
  [ HH.div
    [ cls mxAuto ]
    [ HH.a
      [ HP.href "#"
      , cls textDark 
      ]
      [ HH.text "购买流程"]
    , HH.text " | "
    , HH.a
      [ HP.href "#"
      , cls textDark
      ]
      [ HH.text "退换货政策" ]
    , HH.text " | "
    , HH.a
      [ HP.href "#"
      , cls textDark
      ]
      [ HH.text "关于我们"]
    ]
  , HH.div
    [ cls mxAuto]
    [ HH.a
      [ HP.href "#"
      , cls textDark
      ]
      [ HH.text "保定祥琴网络科技有限公司" ]
    ]
  , HH.div
    [ cls mxAuto ] 
    [ HH.text "冀ICP备19001701号-1" ]
  ]



getOrElse :: forall a. Maybe a -> a -> a
getOrElse Nothing val = val
getOrElse (Just val) _ = val


renderModal :: ∀ p i. String -> String -> Array (H.HTML p i) -> Array (H.HTML p i) -> H.HTML p i 
renderModal id title footer body = 
  HH.div 
  [ cls $ modal <> fade
  , HP.tabIndex (-1)
  , "role" ->> "dialog"
  , "id" ->> id
  ]
  [ HH.div
    [ cls modalDialog 
    , "role" ->> "document"
    ]
    [ HH.div
      [ cls modalContent ]
      [ HH.div 
        [ cls modalContent ]
        [ HH.div 
          [ cls modalHeader ]
          [ HH.h5 
            [ cls modalTitle ]
            [ HH.text title ]
          , HH.button
            [ cls close
            , "type" ->> "button"
            , "data-dismiss" ->> "modal"
            , "aria-label" ->> "Close"
            ]
            [ HH.span
              [ "aria-hidden" ->> "true" ]
              [ HH.text "×" ]
            ]
          ]
        , HH.div 
          [ cls modalBody ]
          body
        , HH.div
          [ cls modalFooter ]
          footer
        ]
      ]
    ]
  ]