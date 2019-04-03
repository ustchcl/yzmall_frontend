module Yzmall.Api.Capablity.Resource.BankCard where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Slug (Slug)
import Yzmall.Data.BankCard (BankCard)


class Monad m <= ManageBankCard m where
  myBankCards :: m (Maybe (Array BankCard))
  getBankCard :: Slug -> m (Maybe BankCard)
  addBankCard :: AddBankCardParams -> m (Maybe BankCard)
  deleteBankCard :: Slug -> m Unit

instance manageBankCardHalogenM :: ManageBankCard m => ManageBankCard (HalogenM s f g p o m) where
  myBankCards = lift myBankCards
  getBankCard = lift <<< getBankCard
  addBankCard = lift<<< addBankCard
  deleteBankCard = lift<<< deleteBankCard


type AddBankCardParams =
  { name :: String
  , cardId :: String
  , region :: String
  , phone :: String
  , bank :: String
  , setDefault :: Boolean
  }