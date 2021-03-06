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
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Type.Equality (class TypeEquals, from)
import Yzmalal.Data.CommodityOrder (decodeArrayCommodityOrder, decodeCommodityOrder, decodePayForOrderResult)
import Yzmall.Api.Capablity.Resource.Account (class ManageAccount, getAccountInfo)
import Yzmall.Api.Capablity.Resource.Address (class ManageAddress)
import Yzmall.Api.Capablity.Resource.BankCard (class ManageBankCard)
import Yzmall.Api.Capablity.Resource.CommodityOrder (class ManageOrder, createACTSellRebate, createOrderSpecial)
import Yzmall.Api.Endpoint (Endpoint(..))
import Yzmall.Api.Request (BaseURL(..), RequestMethod(..))
import Yzmall.Api.Utils (decode, mkRequest)
import Yzmall.Capability.LogMessages (class LogMessages)
import Yzmall.Capability.Navigate (class Navigate, navigate)
import Yzmall.Capability.Now (class Now)
import Yzmall.Data.ACTSell (decodeACTSell, decodeArrayACTSell, decodePrice, decodeMytSharedRecord)
import Yzmall.Data.Account (Account, decodeAccount, decodeArrayAccount)
import Yzmall.Data.Address (decodeAddress, decodeArrayAddress)
import Yzmall.Data.BankCard (decodeArrayBankCard, decodeBankCard)
import Yzmall.Data.Commission (decodeArrayCommission)
import Yzmall.Data.Commodity (decodeArrayCommodity, decodeCommodity)
import Yzmall.Data.Log as Log
import Yzmall.Data.Route (Route)
import Yzmall.Data.Route as Route
import Yzmall.Resource.Commodity (class ManageCommodity, getCommodities, getCommodity)

type Env = 
  { logLevel :: LogLevel
  , baseUrl :: BaseURL
  , currentAccount :: Ref (Maybe Account)
  , currentAddressId :: Ref (Maybe Int)
  , currentBankCardId :: Ref (Maybe Int)
  , lastRoute :: Ref (Maybe Route)
  }

data LogLevel = Dev | Prod

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

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do 
    env <- ask 
    liftEffect case env.logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance navigateAppM :: Navigate AppM where
  navigate = 
    liftEffect <<< setHash <<< print Route.routeCodec

  logout = do
    liftEffect <<< Ref.write Nothing =<< asks _.currentAccount
    navigate $ Route.RegularCommodity Route.LoginHome

instance managerCommodityAppM :: ManageCommodity AppM where
  getCommodities params = 
    mkRequest { endpoint: ViewCommodity params, method: Get  }
      >>= decode decodeArrayCommodity
  getCommodity slug = 
    mkRequest { endpoint: GetCommodity slug, method: Get }
      >>= decode decodeCommodity

  viewCommissions = 
    mkRequest { endpoint: ViewCommissions, method: Get }
      >>= decode decodeArrayCommission

{-
class Monad m <= ManageAccount m where
  getAddresses :: m (Maybe (Array Address))
  getBankCards :: m (Maybe (Array BankCard))
  addBankCard :: BankCardFields -> m (Maybe BankCard)
  addAddress :: AddressFields -> m (Maybe Address)
  getAccountInfo :: m (Maybe Account)
  setIDCard :: IDCard -> m (Maybe Account)
-}
instance manageAccountAppM :: ManageAccount AppM where
  getAccountInfo = mkRequest { endpoint: MyAccountInfo, method: Get }
    >>= decode decodeAccount
    
  login loginFeilds = do
    let method = Post $ Just $ encodeJson loginFeilds
    mkRequest { endpoint: Login, method: method }
      >>= decode decodeAccount

  createAccount params = do
    let method = Post $ Just $ encodeJson params
    mkRequest { endpoint: CreateAccount, method: method }
      >>= decode decodeAccount

  createVcode phone = do 
    let method = Post $ Just $ encodeJson {phone}
    void $ mkRequest { endpoint: CreateVcode, method: method }

  setName name idCard = do
    let method = Post $ Just $ encodeJson {name, idCard}
    mkRequest { endpoint: SetName, method: method }
      >>= decode decodeAccount 
  
  resetPassword params = do 
    let method = Post $ Just $ encodeJson params
    mkRequest { endpoint: ResetPassword, method: method }
      >>= decode decodeAccount

  getInvitees = 
    mkRequest { endpoint: Invitees, method: Get }
      >>= decode decodeArrayAccount

  logout = do
    void $ mkRequest { endpoint: Logout, method: Post Nothing }
    liftEffect <<< Ref.write Nothing =<< asks _.currentAccount
    navigate $ Route.RegularCommodity Route.NormalHome
  
  bindAlipay alipay = do
    let method = Post $ Just $ encodeJson { alipay }
    mkRequest { endpoint: BindAlipay, method: method }
      >>= decode decodeAccount

instance manageAddressAppM :: ManageAddress AppM where
  myAddresses  = 
    mkRequest { endpoint: MyAddresses, method: Get }
     >>= decode decodeArrayAddress

  getAddress slug = 
    mkRequest { endpoint: GetAddress slug, method: Get }
     >>= decode decodeAddress

  deleteAddress slug = 
    void $ mkRequest { endpoint: DeleteAddress slug, method: Post Nothing}

  addAddress params =
    mkRequest { endpoint: AddAddress, method: Post $ Just $ (encodeJson params) } 
      >>= decode decodeAddress


instance manageBankCardAppM :: ManageBankCard AppM where
  myBankCards  = 
    mkRequest { endpoint: MyBankCards, method: Get }
     >>= decode decodeArrayBankCard

  getBankCard slug = 
    mkRequest { endpoint: GetBankCard slug, method: Get }
     >>= decode decodeBankCard

  deleteBankCard slug = 
    void $ mkRequest { endpoint: DeleteBankCard slug, method: Post Nothing}

  addBankCard params =
    mkRequest { endpoint: AddBankCard, method: Post $ Just $ (encodeJson params) } 
      >>= decode decodeBankCard

instance manageOrderAppM :: ManageOrder AppM where
  viewOrder =
    mkRequest { endpoint: ViewOrder, method: Get }
      >>= decode decodeArrayCommodityOrder

  payForOrder slug vcode' = 
    let 
      mbObj (Just vcode) = Just { vcode }
      mbObj Nothing = Nothing
    in
    mkRequest { endpoint: PayForOrder slug, method: Post $ encodeJson <$> mbObj vcode'}
      >>= decode decodePayForOrderResult
  
  createOrderSpecial slug params = 
    mkRequest { endpoint: CreateOrderSpecial slug, method: Post $ Just $ encodeJson params }
      >>= decode decodeCommodityOrder

  payForOrderSpecial slug vcode' = 
    let 
      mbObj (Just vcode) = Just { vcode }
      mbObj Nothing = Nothing
    in
    mkRequest { endpoint: PayForOrderSpecial slug, method: Post $ encodeJson <$> mbObj vcode' }
      >>= decode decodePayForOrderResult
  
  deleteOrder slug = 
    void $ mkRequest { endpoint: DeleteOrder slug, method: Post Nothing }
  
  createACTSellCommission amount = 
    mkRequest { endpoint: CreateACTSellCommission, method: Post $ Just $ encodeJson {amount} }
      >>= decode decodeACTSell
  
  createACTSellRebate amount = 
    mkRequest { endpoint: CreateACTSellRebate, method: Post $ Just $ encodeJson {amount} }
      >>= decode decodeACTSell
  
  createMYTSellRush = 
    mkRequest { endpoint: CreateACTSellRush, method: Post Nothing }
      >>= decode decodeACTSell
  
  createOrder slug params = 
    mkRequest { endpoint: CreateOrder slug, method: Post $ Just $ encodeJson params }
      >>= decode decodeCommodityOrder

  mytSharedRecord = 
    mkRequest { endpoint: ACTSharedRecord , method : Get }
      >>= decode decodeMytSharedRecord
    
  getACTSells = 
    mkRequest { endpoint: MyACTSells , method : Get }
      >>= decode decodeArrayACTSell