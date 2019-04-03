module Yzmall.Api.Capablity.Resource.Account where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Yzmall.Api.Request (LoginFields)
import Yzmall.Data.Account (Account, RegisterParams, ResetPasswordParams)


class Monad m <= ManageAccount m where
  getAccountInfo :: m (Maybe Account)
  login :: LoginFields -> m (Maybe Account)
  createAccount :: RegisterParams -> m (Maybe Account) 
  createVcode :: String -> m Unit
  setName :: String -> String -> m (Maybe Account)
  resetPassword :: ResetPasswordParams -> m (Maybe Account)
  getInvitees :: m (Maybe (Array Account))
  logout :: m Unit 
  bindAlipay :: String -> m (Maybe Account)

instance manageAccountHalogenM :: ManageAccount m => ManageAccount (HalogenM s f g p o m) where
  getAccountInfo = lift getAccountInfo
  login = lift <<< login
  createAccount = lift <<< createAccount
  createVcode= lift <<< createVcode
  setName p = lift <<< setName p
  resetPassword = lift <<< resetPassword
  getInvitees = lift getInvitees
  logout = lift logout
  bindAlipay = lift <<< bindAlipay

