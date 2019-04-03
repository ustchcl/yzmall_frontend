module Yzmall.Api.Utils where

import Prelude

import Affjax (Response, request)
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Either (Either(..), hush, note)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Web.XHR.XMLHttpRequest (response)
import Yzmall.Api.Endpoint (Endpoint(..))
import Yzmall.Api.Request (BaseURL(..), RequestOptions, defaultRequest, defaultRequestForm)
import Yzmall.Capability.LogMessages (class LogMessages, logError)
import Yzmall.Capability.Now (class Now)
import Yzmall.Data.ErrorMsg (decodeErrorMsg)
import Yzmall.Page.Utils (alertMsg, unsafeAlert)

mkRequest 
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- ask
  response <- liftAff $ request $ defaultRequestForm baseUrl opts
  let 
    status = response.status
    body = hush response.body
  liftEffect $ handleError status (note "sth." body) opts.endpoint
  pure body

handleError :: StatusCode -> Either String Json  -> Endpoint -> Effect Unit
handleError status body endpoint = do
  case status of 
    (StatusCode 200) -> pure unit
    _ ->
      let 
        errMsg = decodeErrorMsg =<< body
      in
      case errMsg of 
        Left _ -> 
          case endpoint of
            Login ->  pure $ unsafeAlert "登录失败, 手机号或密码不正确"
            _ -> pure unit 
        Right err -> pure $ unsafeAlert err.content
  pure unit


-- | We can decode records and primitive types automatically, and we've defined custom decoders for
-- | our custom data types. However, our API frequently returns those data structures wrapped in 
-- | a larger object with a single field like "user", "profile", or "article". This utility allows
-- | us to decode a JSON object with a particular key, and then decode the contents. 
-- |
-- | For example, consider this JSON object containing a single field, "user", which itself contains 
-- | a JSON object representing a user profile:
-- |
-- | ```json
-- | { "user": { "username": ... } }
-- | ```
-- | 
-- | We can make our `Profile` decoder compatible with this new JSON using our `decodeAt` helper:
-- |
-- | ```purescript
-- | decodeProfile :: Json -> Either String Profile
-- | decodeProfile = decodeAt "user"
-- | ```
decodeAt :: forall a. DecodeJson a => String -> Json -> Either String a
decodeAt key = decodeJson <=< (_ .: key) <=< decodeJson

-- | This small utility decodes JSON and logs any failures that occurred, returning the parsed 
-- | value only if decoding succeeded. This utility makes it easy to abstract the mechanices of 
-- | dealing with malformed responses. See `Conduit.AppM` for examples of this in practice.
decode :: forall m a. LogMessages m => Now m => (Json -> Either String a) -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing 
decode decoder (Just json) = case decoder json of
  Left err -> logError err *> pure Nothing
  Right response -> pure (Just response)

decodeAs :: forall a. DecodeJson a => Json -> Either String a
decodeAs = decodeJson