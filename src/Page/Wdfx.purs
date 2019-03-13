module Yzmall.Page.Wdfx where

import Halogen.Themes.Bootstrap4 hiding (show)
import Prelude

import Control.Monad.State (state)
import Data.Maybe (Maybe(..))
import Formless (initial)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import String (bgCommodity, sharePng)
import Yzmall.Page.Part.Login (renderLoginModal, renderRegisterModal)
import Yzmall.Page.Part.MobileMenu (forMobileMenu, mobileMenu)
import Yzmall.Utils (CardInfo, cls, renderBanner, renderFooter, renderHeader, renderNavBar, style, (->>))

type InviterID = Int

type State = Int

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
  initialState = 1770724

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
          [ HH.img 
            [ HP.src $ "http://xiangqingkeji.com/poster/" <> show state <> ".jpg"
            , cls w100  
            ] 
          ]
        , renderFooter
        , forMobileMenu
        , mobileMenu
        , renderLoginModal
        , renderRegisterModal
        ]
      