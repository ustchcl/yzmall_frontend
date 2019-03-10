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
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Hash (getHash)
import Yzmall.Api.Capablity.Resource.Account (getAccountInfo, login)
import Yzmall.Api.Endpoint (Endpoint(..))
import Yzmall.Api.Request (BaseURL(..), RequestMethod(..), defaultRequest, defaultRequestForm)
import Yzmall.Api.Utils (decode, decodeAs, decodeAt, mkRequest)
import Yzmall.AppM (LogLevel(..), Env)
import Yzmall.Capability.LogMessages (logDebug, logError)
import Yzmall.Data.Account (Account, decodeAccount)
import Yzmall.Page.Commodity (component)
import Yzmall.Page.PersonalCenter as PC

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

  -- let
  --   environment :: Env
  --   environment = { currentAccount, baseUrl, logLevel }

  -- rootComponent :: H.Commponent HH.HTML Router 
  -- accountInfo <- liftAff $ mkRequest { endpoint: MyAccountInfo, method: Get}  >>= decode 
  -- logDebug accountInfo
  runUI (PC.component Nothing) unit body



loginTest :: Effect (Maybe { endpoint :: Endpoint, method :: RequestMethod })
loginTest = do
  let 
    loginFields = { phone: "123", password: "123" }
    method = Post $ Just $ encodeJson { loginFields }
  pure $ Just { endpoint : MyAccountInfo, method: Get }
  

testLog :: Either String Account -> Effect Unit
testLog (Left err) = log err
testLog (Right acc) = log $ show acc.phone