module Yzmall.Api.Capablity.Resource.Account where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Yzmall.Api.Request (LoginFields)
import Yzmall.Data.Account (Account)


class Monad m <= ManageAccount m where
  getAccountInfo :: m (Maybe Account)
  login :: LoginFields -> m (Maybe Account)

instance manageAccountHalogenM :: ManageAccount m => ManageAccount (HalogenM s f g p o m) where
  getAccountInfo = lift getAccountInfo
  login = lift <<< login

