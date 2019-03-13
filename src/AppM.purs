-- | A custom application monad that provides concrete implementations for capabilities like
-- | logging, navigation, and resource management. This is our production monad -- it interprets
-- | our capabilities as they are meant to run on our production site. 
-- |
-- | However, since capabilities like logging are implemented as type classes, we can also provide 
-- | one or more test monads that provide different interpretations.
-- |
-- | For example, this monad will actually hit the server with API requests when we manage a 
-- | resource, but our test monad might just return mock JSON or error responses.
-- |
-- | See the various `Conduit.Capability.*` modules for deeper explanations of each capability, and
-- | the accompanying guide for a thorough introduction to this style of application architecture.
-- |
-- | https://thomashoneyman.com/guides/real-world-halogen
module Yzmall.AppM where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT(..), ask, asks, runReaderT)
import Data.Argonaut.Encode (encodeJson)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now as Now
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.HTML.Properties (method)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount, getAccountInfo)
import Yzmall.Api.Endpoint (Endpoint(..))
import Yzmall.Api.Request (BaseURL(..), RequestMethod(..))
import Yzmall.Api.Utils (decode, mkRequest)
import Yzmall.Capability.LogMessages (class LogMessages)
import Yzmall.Capability.Navigate (class Navigate, navigate)
import Yzmall.Capability.Now (class Now)
import Yzmall.Data.Account (Account, decodeAccount)
import Yzmall.Data.Log as Log
import Yzmall.Data.Route as Route

type Env = 
  { logLevel :: LogLevel
  , baseUrl :: BaseURL
  , currentAccount :: Ref (Maybe Account)
  }

data LogLevel = Dev | Prob

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

-- instance logMessagesAppM :: LogMessages AppM where
--   logMessage log = do 
--     env <- ask
--     liftEffect case env.logLevel, Log.reason log of
--       Prod, Log.Debug -> pure unit
--       _, _ -> Console.log $ Log.message log

instance navigateAppM :: Navigate AppM where
  navigate = 
    liftEffect <<< setHash <<< print Route.routeCodec

  logout = do
    liftEffect <<< Ref.write Nothing =<< asks _.currentAccount
    navigate Route.Home

{-
class Monad m <= ManageAccount m where
  getAddresses :: m (Maybe (Array Address))
  getBankCards :: m (Maybe (Array BankCard))
  addBankCard :: BankCardFields -> m (Maybe BankCard)
  addAddress :: AddressFields -> m (Maybe Address)
  getAccountInfo :: m (Maybe Account)
  setIDCard :: IDCard -> m (Maybe Account)
-}
-- instance manageAccountAppM :: ManageAccount AppM where
--   getAccountInfo = mkRequest { endpoint: MyAccountInfo, method: Get }
--     >>= decode decodeAccount
    
--   login loginFeilds = do
--     let method = Post $ Just $ encodeJson { loginFeilds }
--     mkRequest { endpoint: Login, method: method }
--       >>= decode decodeAccount