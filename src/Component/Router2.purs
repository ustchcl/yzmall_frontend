-- | The `Router` component is the root of our Halogen application. Every other component is a 
-- | direct descendent of this component. We'll use the router to choose which component to render
-- | given a particular `Route` and to manage the user's location in the application.
-- |
-- | See `Main` to understand how this component is used as the root of the application.
module Yzmall.Component.Router2 where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Either (Either)
import Data.Either.Nested (Either1, Either10, Either11, Either12, Either5, Either6, Either7, Either8, Either9, Either4)
import Data.Functor.Coproduct (Coproduct(..))
import Data.Functor.Coproduct.Nested (Coproduct10, Coproduct11, Coproduct12, Coproduct5, Coproduct6, Coproduct7, Coproduct8, Coproduct9, Coproduct4)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Slug (toString)
import Type.Data.Boolean (kind Boolean)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount)
import Yzmall.Api.Capablity.Resource.Address (class ManageAddress)
import Yzmall.Api.Capablity.Resource.BankCard (class ManageBankCard)
import Yzmall.Api.Capablity.Resource.CommodityOrder (class ManageOrder)
import Yzmall.Capability.LogMessages (class LogMessages)
import Yzmall.Capability.Navigate (class Navigate)
import Yzmall.Capability.Now (class Now)
import Yzmall.Data.Account (Account)
import Yzmall.Data.Route (Route)
import Yzmall.Data.Route2 (Route2(..))
import Yzmall.Page.AccountInfo as AI
import Yzmall.Page.AddressEditor as AE
import Yzmall.Page.AddressSelector as AS
import Yzmall.Page.BankCardSelector as BS
import Yzmall.Page.BankCardEditor as BCE
import Yzmall.Page.MyOrders as MO
import Yzmall.Page.PersonalCenter as PC
import Yzmall.Resource.Commodity (class ManageCommodity)

type State =
  { route :: Route2
  , clear :: Boolean
  }

data Query a
  = Navigate Route2 a
  | Reset Route2 a 
  | Clear a

type Input =
  Maybe Route2

-- If you haven't seen nested `Coproduct` or `Either` before, or you haven't worked with multiple types of
-- child component, then these types are probably confusing. They're a little tedious to define and are
-- being removed in favor of a much nicer mechanism in Halogen 5, but are necessary in Halogen 4.
-- 
-- For a detailed explanation of what's going on here, please see this issue:
-- https://github.com/thomashoneyman/purescript-halogen-realworld/issues/20
type ChildQuery = 
    Coproduct7
      PC.Query
      AI.Query
      AE.Query
      BCE.Query
      MO.Query
      AS.Query
      BS.Query
      
    

type ChildSlot =  
    Either7
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
    { initialState: \initialRoute -> { route: fromMaybe (PersonalCenter) initialRoute, clear : false } 
    , render
    , eval
    , receiver: const Nothing
    }

  where 

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval  = case _ of 
    (Navigate dest a) -> do
      { route } <- H.get 
      when (route /= dest) do
        H.modify_ _ { route = dest }
      pure a
    (Reset dest a) -> do
      { route } <- H.get 
      when (route /= dest) do
        H.modify_ _ { route = dest }
      pure a
    Clear a -> do
      H.modify_ _ {clear = true}
      pure a
    

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render { route, clear } = 
      case route of
        PersonalCenter -> 
          HH.slot' CP.cp1 unit PC.component unit absurd
        AccountInfo ->
          HH.slot' CP.cp2 unit AI.component unit absurd
        AddressEditor ->
          HH.slot' CP.cp3 unit AE.component unit absurd
        BankCardEditor ->
          HH.slot' CP.cp4 unit BCE.component unit absurd
        MyOrders -> 
          HH.slot' CP.cp5 unit MO.component unit absurd
        AddressSelector -> 
          HH.slot' CP.cp6 unit AS.component unit absurd
        BankCardSelector -> 
          HH.slot' CP.cp7 unit BS.component unit absurd


