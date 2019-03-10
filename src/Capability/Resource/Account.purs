module Yzmall.Api.Capablity.Resource.Account where

import Prelude

import Data.Date (Month(..))
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Yzmall.Data.Account (Account)
import Yzmall.Data.BaseTypes (Address, AddressFields, BankCard, BankCardFields, IDCard, LoginFields)


class Monad m <= ManageAccount m where
  -- getAddresses :: m (Maybe (Array Address))
  -- getBankCards :: m (Maybe (Array BankCard))
  -- addBankCard :: BankCardFields -> m (Maybe BankCard)
  -- addAddress :: AddressFields -> m (Maybe Address)
  getAccountInfo :: m (Maybe Account)
  -- setIDCard :: IDCard -> m (Maybe Account)
  login :: LoginFields -> m (Maybe Account)