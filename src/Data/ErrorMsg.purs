module Yzmall.Data.ErrorMsg where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Either (Either)

type ErrorMsg = 
  { errorCode :: Int
  , content :: String
  }

decodeErrorMsg :: Json -> Either String ErrorMsg
decodeErrorMsg json = do
  obj <- decodeJson json
  errorCode <- obj .: "errorCode"
  content <- obj .: "content"
  pure { content, errorCode }