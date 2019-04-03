-- | The `Router` component is the root of our Halogen application. Every other component is a 
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
-- |
-- | See `Main` to understand how this component is used as the root of the application.
module Yzmall.Component.Router where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (Either)
import Data.Either.Nested (Either1, Either10, Either11, Either5, Either6, Either7, Either8, Either9, Either12)
import Data.Functor.Coproduct (Coproduct(..))
import Data.Functor.Coproduct.Nested (Coproduct10, Coproduct11, Coproduct12, Coproduct5, Coproduct6, Coproduct7, Coproduct8, Coproduct9)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Slug (toString)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount)
import Yzmall.Api.Capablity.Resource.Address (class ManageAddress)
import Yzmall.Api.Capablity.Resource.BankCard (class ManageBankCard)
import Yzmall.Api.Capablity.Resource.CommodityOrder (class ManageOrder)
import Yzmall.Capability.LogMessages (class LogMessages)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Capability.Now (class Now)
import Yzmall.Component.Router2 as Router2
import Yzmall.Data.Account (Account)
import Yzmall.Data.Route (HomeType(..), Route(..))
import Yzmall.Data.Route2 (Route2(..))
import Yzmall.Page.Commodity as RS
import Yzmall.Page.CommodityInfo as CDI
import Yzmall.Page.PayOrderPage as POP
import Yzmall.Page.PurchaseConfirm as PHC
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
type ChildQuery = 
    Coproduct9
      RS.Query
      PHC.Query
      CDI.Query
      WDDS.Query
      WDTG.Query
      WDFX.Query
      TC.Query
      Router2.Query
      POP.Query
      
    

type ChildSlot =  
    Either9
      Unit
      Unit
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
  => MonadAsk { currentAccount :: Ref (Maybe Account), currentAddressId :: Ref (Maybe Int), currentBankCardId :: Ref (Maybe Int), lastRoute :: Ref (Maybe Route) | r } m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageAccount m
  => ManageCommodity m
  => ManageOrder m
  => ManageAddress m
  => ManageBankCard m
  -- => ManageComment m
  -- => ManageTag m
  => H.Component HH.HTML Query Input Void m
component =
  H.parentComponent
    { initialState: \initialRoute -> { route: fromMaybe (Home true NormalHome) initialRoute } 
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
      { lastRoute } <- ask
      liftEffect $ Ref.write (Just route) lastRoute
      case dest of 
        RegularCommodity ht -> do
          _ <- H.query' CP.cp1 unit (H.action $ RS.Reset true ht)
          pure unit 
        SpecialCommodity ht -> do
          _ <- H.query' CP.cp1 unit (H.action $ RS.Reset false ht)
          pure unit
        PC_ROUTER st -> do
          _ <- H.query' CP.cp8 unit (H.action $ Router2.Navigate (fromMaybe PersonalCenter st))
          pure unit
        _ -> pure unit
    pure a

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render { route } = case route of
    Home isRegular hometype -> 
      HH.slot' CP.cp1 unit RS.component {isRegular, hometype} absurd
    RegularCommodity hometype -> 
      HH.slot' CP.cp1 unit RS.component {isRegular: true, hometype} absurd
    SpecialCommodity hometype -> 
      HH.slot' CP.cp1 unit RS.component {isRegular: false, hometype} absurd
    PC_ROUTER route2  -> 
      HH.slot' CP.cp8 unit Router2.component route2 absurd
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
    PurchaseConfirm slug -> 
      HH.slot' CP.cp2 unit PHC.component { slug, isSpecial: false } absurd
    SpecialPurchaseConfirm slug -> 
      HH.slot' CP.cp2 unit PHC.component { slug, isSpecial: true } absurd
    PayOrder params -> 
      HH.slot' CP.cp9 unit POP.component params absurd
    _ -> 
      HH.slot' CP.cp1 unit RS.component {isRegular: true, hometype: NormalHome} absurd

