-- | The `Router` component is the root of our Halogen application. Every other component is a 
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
-- |
-- | See `Main` to understand how this component is used as the root of the application.
module Yzmall.Component.Router where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Either.Nested (Either5, Either7, Either6)
import Data.Functor.Coproduct.Nested (Coproduct5, Coproduct7, Coproduct6)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount)
import Yzmall.Capability.LogMessages (class LogMessages)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Capability.Now (class Now)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Route (Route(..))
import Yzmall.Page.Commodity as RS
import Yzmall.Page.CommodityInfo as CDI
import Yzmall.Page.PersonalCenter as PC
import Yzmall.Page.TradeCenter as TC
import Yzmall.Page.Wdds as WDDS
import Yzmall.Page.Wdfx as WDFX
import Yzmall.Page.Wdtg as WDTG
import Yzmall.Resource.Commodity (class ManageCommodity)

type State =
  { route :: Route }

data Query a
  = Navigate Route a

type Input =
  Maybe Route

-- If you haven't seen nested `Coproduct` or `Either` before, or you haven't worked with multiple types of
-- child component, then these types are probably confusing. They're a little tedious to define and are
-- being removed in favor of a much nicer mechanism in Halogen 5, but are necessary in Halogen 4.
-- 
-- For a detailed explanation of what's going on here, please see this issue:
-- https://github.com/thomashoneyman/purescript-halogen-realworld/issues/20
type ChildQuery = Coproduct7
  RS.Query
  PC.Query
  CDI.Query
  WDDS.Query
  WDTG.Query
  WDFX.Query
  TC.Query

type ChildSlot = Either7
  Unit
  Unit
  Unit
  Unit
  Unit
  Unit
  Unit

component
  :: forall m r
   . MonadAff m
  => MonadAsk { currentAccount :: Ref (Maybe Account) | r } m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageAccount m
  => ManageCommodity m
  -- => ManageComment m
  -- => ManageTag m
  => H.Component HH.HTML Query Input Void m
component =
  H.parentComponent
    { initialState: \initialRoute -> { route: fromMaybe Home initialRoute } 
    , render
    , eval
    , receiver: const Nothing
    }

  where 

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (Navigate dest a) = do
    { route } <- H.get 
    when (route /= dest) do
      H.modify_ _ { route = dest }
    pure a

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render { route } = case route of
    RegularCommodity -> 
      HH.slot' CP.cp1 unit RS.component unit absurd
    AccountInfo -> 
      HH.slot' CP.cp2 unit (PC.component Nothing) unit absurd
    CommodityInfo slug -> 
      HH.slot' CP.cp3 unit CDI.component { slug } absurd
    WDDS_ROUTE -> 
      HH.slot' CP.cp4 unit WDDS.component unit absurd
    WDTG_ROUTE -> 
      HH.slot' CP.cp5 unit WDTG.component unit absurd
    WDFX_ROUTE -> 
      HH.slot' CP.cp6 unit WDFX.component unit absurd
    TradeCenter -> 
      HH.slot' CP.cp7 unit TC.component unit absurd
    _ -> 
      HH.slot' CP.cp1 unit RS.component unit absurd

