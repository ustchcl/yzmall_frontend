module Main where

import Prelude

import Affjax (printResponseFormatError, request)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Foldable (traverse_)
import Data.HTTP.Method (Method)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (getHash, matchesWith)
import Yzmall.Api.Capablity.Resource.Account (getAccountInfo, login)
import Yzmall.Api.Endpoint (Endpoint(..))
import Yzmall.Api.Request (BaseURL(..), RequestMethod(..), defaultRequest, defaultRequestForm)
import Yzmall.Api.Utils (decode, decodeAs, decodeAt, mkRequest)
import Yzmall.AppM (Env, LogLevel(..), runAppM)
import Yzmall.Capability.LogMessages (logDebug, logError)
import Yzmall.Component.Router as Router
import Yzmall.Data.Account (Account, decodeAccount)
import Yzmall.Data.Route (Route, routeCodec)
import Yzmall.Page.Utils (initCommodities)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let 
    -- baseUrl = BaseURL "http://192.168.0.138:8080/yzmall-server"
    baseUrl = BaseURL "http://www.scix.vip/yzmall-server"
    logLevel = Dev

  liftEffect $ initCommodities
  -- 
  currentAccount <- liftEffect $ Ref.new Nothing
  currentAddressId <- liftEffect $ Ref.new Nothing
  currentBankCardId <- liftEffect $ Ref.new Nothing
  lastRoute <- liftEffect $ Ref.new Nothing
  initialHash <- liftEffect $ getHash

  -- 尝试登陆
  
  liftEffect tryLogin >>= traverse_ \op -> do
    res <- liftAff $ request $ defaultRequest baseUrl op
    let u = decodeAccount =<< lmap printResponseFormatError res.body
    liftEffect $ testLog u
    liftEffect $ Ref.write (hush u) currentAccount
    pure unit

  let
    environment :: Env
    environment = { currentAccount, baseUrl, logLevel, currentAddressId, currentBankCardId, lastRoute }

    rootComponent :: H.Component HH.HTML Router.Query Router.Input Void Aff
    rootComponent = H.hoist (runAppM environment) Router.component

    initialRoute :: Maybe Route
    initialRoute = hush $ parse routeCodec initialHash

  halogenIO <- runUI rootComponent initialRoute body
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.action $ Router.Navigate new
  
  pure unit




tryLogin :: Effect (Maybe { endpoint :: Endpoint, method :: RequestMethod })
tryLogin = do
  pure $ Just { endpoint : MyAccountInfo, method: Get }
  

testLog :: Either String Account -> Effect Unit
testLog (Left err) = log err
testLog (Right acc) = log $ show acc.defaultAddress