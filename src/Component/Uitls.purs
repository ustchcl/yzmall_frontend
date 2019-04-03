module Conduit.Component.Utils where

import Prelude

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)
import Yzmall.Capability.Navigate (class Navigate, logout, navigate)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Route (Route, routeCodec)
import Yzmall.Data.Route as Route

-- | I get annoyed writing `class_ $ ClassName "..."` over and over again. This small utility saves
-- | a few characters all over our HTML.
css :: forall r i. String -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName

-- | We must provide a `String` to the "href" attribute, but we represent routes with the much
-- | better `Route` type. This utility is a drop-in replacement for `href` that uses `Route`.
safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< print routeCodec

-- | Sometimes we need to deal with elements which may or may not exist. This function lets us
-- | provide rendering for the element if it exists, and renders an empty node otherwise.
maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text ""

maybeElemArray :: forall p i a. Maybe a -> (a -> Array (HH.HTML p i)) -> Array (HH.HTML p i)
maybeElemArray (Just x) f = f x
maybeElemArray _ _ = [ HH.text "" ]

-- | PureScript is a strict language. If we want to conditionally display an element, then we
-- | should hide the evaluation behind a function, which won't be evaluated right away, in order
-- | to minimize the work performed each render.
whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""


guardAccount 
  :: forall m r
   . MonadEffect m 
  => MonadAsk { currentAccount :: Ref (Maybe Account) | r } m
  => Navigate m 
  => m (Maybe Account)
guardAccount = do
 asks _.currentAccount >>= (Ref.read >>> liftEffect) >>= case _ of
  Nothing -> logout *> pure Nothing
  Just account -> 
    case account.aliPay of 
      Nothing -> (navigate $ Route.RegularCommodity Route.BindAlipay) *> pure (Just account)
      otherwise -> pure (Just account)



