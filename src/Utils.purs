module Yzmall.Utils where
import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Conduit.Component.Utils (safeHref)
import Data.Array (take, takeEnd, zip, (..))
import Data.Maybe (Maybe(..), fromJust)
import Data.String (fromCodePointArray, toCodePointArray)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Slug (Slug, generate)
import String (bgBanner, bgFooter, bgHeader, imgUrl, logoNavbar, logoTitle)
import Web.HTML.Event.EventTypes (cancel)
import Yzmall.Data.Commodity (Commodity, CommodityCategory(..))
import Yzmall.Data.Route (HomeType(..), Route(..), testSlug)

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

renderCommodity :: forall p i. Commodity -> Int -> H.HTML p i
renderCommodity com n = 
  let pLeftOrRight = if n `mod` 2 == 0 then H.ClassName "pr-md-1 pl-md-0" else H.ClassName "pr-md-0 pl-md-1"
  in
  HH.a
  [ cls $ colMd6 <> card <> my2 <> border0 <> px0 <> pLeftOrRight <> bgTransparent
  , safeHref (CommodityInfo $ unsafePartial $ fromJust (generate $ show com.id))
  , style "text-decoration:none;"
  ]
  [ HH.div 
    [ cls $ cardImgTop ]
    ([ HH.img
      [  cls $ cardImgTop <> pLeftOrRight
      , HP.src com.thumbnail
      ]
    ] <> if (com.stock > 0) then [] else [
        HH.img
        [ cls $ cardImgTop <> cardImgOverlay <> p0 <> pLeftOrRight
        , style "position: absolute"
        , HP.src "./image/none.png"
        ]
    ])
    
  , HH.div
    [ cls $ cardBody <> row <> px0 <> py1 <> alignMiddle ]
    [ HH.div
      [ cls $ col9]
      [ HH.p
        [ cls $ cardText <> textTruncate <> w100 <> pl1
        , "style" ->> "color: #DFDBC6"
        ]
        [ HH.text com.name ]
      ]
    , HH.div
      [ cls $ col3  ]
      [ HH.small
        [ "style" ->> "color: #DFDBC6"
        , cls $ floatRight <> pr1 <> pt1]
        [ HH.text $ "销量:" <> show com.sale]
      ]
    , HH.div
      [cls col6]
      [ HH.b
        [ cls $ cardText <> w100 <> pl1
        , style "font-size: 150%; color: #F6C17F" 
        ]
        [HH.text $ "￥" <> show com.price]
      ]
    -- , HH.div
    --   [cls col6]
    --   [ HH.b
    --     [ cls $ floatRight <> pr1 <> pt1
    --     , style "color: #F6C17F" 
    --     ]
    --     [HH.text $ "(" <> (if com.category == Regular then "赠送" else "消耗") <> show com.gold <> "金币)"]
    --   ]
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
renderBanner :: forall p i. HH.HTML p i
renderBanner =
      HH.a
        [ HP.class_ $ mxAuto <> myAuto ]
        [
          HH.div
            [ HP.class_ $ carousel <> (H.ClassName "slide")
            , attr_ "data-ride" "carousel"
            , attr_ "id" "carouselControls"
            ]
            [ HH.ol
              [ HP.class_ carouselIndicators]
              [ renderli "carouselControls" 0]
            , HH.div
                [ HP.class_ carouselInner ]
                [ renderSlider "./image/banner_01.jpeg" "https://mp.weixin.qq.com/s/xBamclrgnW5YDzCyLQFavw" ]
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

renderSlider :: forall p i. String -> String -> HH.HTML p i
renderSlider src url =
    HH.a
      [ HP.class_ $ carouselItem <> active 
      , HP.href url
      ] 
      [ HH.img
        [ HP.class_ $ dBlock <> w100
        , HP.src src
        , HP.alt $ "slider-1" 
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

{-

<form class="form-inline my-2 my-lg-0">
      <input class="form-control mr-sm-2" type="search" placeholder="Search" aria-label="Search">
      <button class="btn btn-outline-success my-2 my-sm-0" type="submit">Search</button>
    </form>
-}

renderHeader ::  ∀ p i. HH.HTML p i
renderHeader = 
  HH.div 
  [ cls $ w100 <> dNone <> H.ClassName "d-md-block" <> bgTransparent
  , style $ "height: 128px; background-image: url(" <> bgHeader <> ")"
  ]
  [ HH.div
    [cls container]
    [ HH.img
      [ HP.src logoTitle ]
    ]
  ]

renderNavBar :: ∀ p i. HH.HTML p i
renderNavBar = 
  HH.nav 
  [ HP.class_  $ navbar <> navbarExpandMd <> navbarDark <>  bgTransparent  <+> "sticky-top" ]
  [ HH.div
    [ cls $ container <> px0]
    [ HH.a 
      [ cls $ navbarBrand
      , safeHref (RegularCommodity NormalHome)
      ]
      [ HH.p
        [ cls m0
        , style "color: #ffe8bb"]
        [ HH.text "银洲国际"]
      ]
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
      [ HH.ul
        [ cls navbarNav ]
        [ renderNavItem  "正价商品" 0 (RegularCommodity NormalHome) 
        , renderNavItem  "特价商品" 1 (SpecialCommodity NormalHome) 
        , renderNavItem  "交易中心" 2 (RegularCommodity NormalHome) 
        , renderNavItem  "个人中心" 3 (RegularCommodity NormalHome) 
        ]
      , HH.button
        [ cls $ btn <> btnOutlineDanger <> my2 <> mlAuto <+> "my-sm-0"
        , "data-toggle" ->> "modal"
        , "data-target" ->> "#loginModal"
        ]
        [ HH.text "登录"]
      , HH.button
        [ cls $ btn <> btnDanger <> my2 <> ml3 <+> "my-sm-0"
        , "data-toggle" ->> "modal"
        , "data-target" ->> "#registerModal" 
        ]
        [ HH.text "注册"]
      ]
    ]
  ]
  
  where
  renderNavItem str n route =
    HH.li
    [ cls $ navItem <> (if n == 0 then active else H.ClassName "")]
    [ HH.a
      [ cls $ navLink 
      , safeHref route
      ]
      [ HH.text str ]
    ]

removerGutter :: String
removerGutter = "padding-left: -15px; margin-right: -15px;"


renderFooter :: ∀ p i. HH.HTML p i
renderFooter =
  HH.text ""
  -- HH.div 
  -- [ cls $ dFlex <> flexColumn <> textDark <> justifyContentCenter
  -- , style $ "height: 160px; padding-top: 2.8rem; font-size: 24; background-position: top center; background-image: url(" <> bgFooter <> ")"
  -- ]
  -- [ HH.div 
  --   [ cls $ mxAuto <> mt2 ]
  --   [ HH.text "2018-2019 Silver Continent International® All Rights Reserved." ] 
  -- HH.div
  --   [ cls mxAuto ]
  --   [ HH.a
  --     [ HP.href "#"
  --     , cls $ textDark
  --     ]
  --     [ HH.text "购买流程"]
  --   , HH.text " | "
  --   , HH.a
  --     [ HP.href "#"
  --     , cls textDark
  --     ]
  --     [ HH.text "退换货政策" ]
  --   , HH.text " | "
  --   , HH.a
  --     [ HP.href "#"
  --     , cls $ textDark
  --     ]
  --     [ HH.text "关于我们"]
  --   ]
  -- , HH.div
  --   [ cls $ mxAuto <> mt2]
  --   [ HH.a
  --     [ HP.href "#"
  --     , cls $ textDark
  --     ]
  --     [ HH.text "保定祥琴网络科技有限公司" ]
  --   ]
  -- , HH.div
  --   [ cls $ mxAuto <> mt2 ] 
  --   [ HH.text "冀ICP备19001701号-1" ]
  -- ]



getOrElse :: forall a. Maybe a -> a -> a
getOrElse Nothing val = val
getOrElse (Just val) _ = val


renderModal :: ∀ p i. String -> String -> Array (H.HTML p i) -> Array (H.HTML p i) -> Boolean-> H.HTML p i 
renderModal id title footer body hideClose = 
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
          , if hideClose then
            (
            HH.button
            [ cls close
            , "type" ->> "button"
            , "data-dismiss" ->> "modal"
            , "aria-label" ->> "Close"
            ]
            [ HH.span
              [ "aria-hidden" ->> "true" ]
              [ HH.text "×" ]
            ]) else (HH.text "")
          ]
        , HH.div 
          [ cls modalBody]
          body
        , HH.div
          [ cls modalFooter ]
          footer
        ]
      ]
    ]
  ]


encodePhone :: String -> String 
encodePhone phone = 
  func (take 3) phone <> "****" <> func (takeEnd 4) phone
  where
  func f = fromCodePointArray <<< f <<< toCodePointArray


-- You have to gurantee the string cannot be empty
unsafeSlug :: String -> Slug
unsafeSlug str = unsafePartial $ fromJust $ generate str

backgroundImage :: String -> String
backgroundImage url = 
  "background-image: url(" <> url <> "); background-position: center center; background-size: contain; background-repeat: no-repeat; "