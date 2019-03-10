module Yzmall.Data.Phone where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)

-- | This type exists purely as an identifier to distinguish it from a normal `String`, so we'll
-- | create a simple newtype which can be freely wrapped or unwrapped.
newtype Phone = Phone String

derive instance newtypePhone :: Newtype Phone _
derive instance genericPhone :: Generic Phone _
derive instance eqPhone :: Eq Phone
derive instance ordPhone :: Ord Phone

derive newtype instance encodeJsonPhone :: EncodeJson Phone
derive newtype instance decodeJsonPhone :: DecodeJson Phone

instance showPhone :: Show Phone where
  show = genericShow
