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
import Effect.Console (log)
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
import Yzmall.Page.Commodity (component)
import Yzmall.Page.CommodityInfo as CDI
import Yzmall.Page.PersonalCenter as PC
import Yzmall.Page.Wdds as WDDS
import Yzmall.Page.Wdtg as WDTG

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let 
    baseUrl = BaseURL "http://192.168.0.138:8080/yzmall-server"
    logLevel = Dev

  -- 
  currentAccount <- liftEffect $ Ref.new Nothing
  initialHash <- liftEffect $ getHash

  -- 尝试登陆
  -- let 
  --   loginFields = { phone: "123", password: "123" }
  --   method = Post $ Just $ encodeJson loginFields
  --   requestOps = { endpoint : Login, method: method }

  -- liftEffect loginTest >>= traverse_ \op -> do
  --   res <- liftAff $ request $ defaultRequest baseUrl op
  --   let account = decodeAt "sss" =<< lmap printResponseFormatError res.body
  --   liftEffect $ Ref.write (hush account) currentAccount
  --   pure unit

  -- _ <- liftAff $ request $ defaultRequestForm baseUrl requestOps
  
  -- liftEffect loginTest >>= traverse_ \op -> do
  --   res <- liftAff $ request $ defaultRequest baseUrl op
  --   let u = decodeAccount =<< lmap printResponseFormatError res.body
  --   liftEffect $ testLog u
    -- liftEffect $ log "request"
    -- liftEffect $ log $ show (_.phone <$> (hush u))
    -- liftEffect $ Ref.write (hush u) currentAccount
    -- pure unit
  -- >>= \json -> do
  --   account <- decode decodeAccount (Just json)
  --   liftEffect $ Ref.write account currentAccount
  --   pure unit

  let
    environment :: Env
    environment = { currentAccount, baseUrl, logLevel }

    rootComponent :: H.Component HH.HTML Router.Query Router.Input Void Aff
    rootComponent = H.hoist (runAppM environment) Router.component

    initialRoute :: Maybe Route
    initialRoute = hush $ parse routeCodec initialHash
  -- accountInfo <- liftAff $ mkRequest { endpoint: MyAccountInfo, method: Get}  >>= decode 
  -- logDebug accountInfo

  halogenIO <- runUI rootComponent initialRoute body
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.action $ Router.Navigate new
  
  pure unit




loginTest :: Effect (Maybe { endpoint :: Endpoint, method :: RequestMethod })
loginTest = do
  let 
    loginFields = { phone: "123", password: "123" }
    method = Post $ Just $ encodeJson { loginFields }
  pure $ Just { endpoint : MyAccountInfo, method: Get }
  

testLog :: Either String Account -> Effect Unit
testLog (Left err) = log err
testLog (Right acc) = log $ show acc.phone