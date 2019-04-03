module Yzmall.Api.Capablity.Resource.Address where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Slug (Slug)
import Yzmall.Data.Address (Address)


class Monad m <= ManageAddress m where
  myAddresses :: m (Maybe (Array Address))
  getAddress :: Slug -> m (Maybe Address)
  addAddress :: AddAddressParams -> m (Maybe Address)
  deleteAddress :: Slug -> m Unit

instance manageAddressHalogenM :: ManageAddress m => ManageAddress (HalogenM s f g p o m) where
  myAddresses = lift myAddresses
  getAddress = lift <<< getAddress
  addAddress = lift<<< addAddress
  deleteAddress = lift<<< deleteAddress


type AddAddressParams =
  { name :: String
  , address :: String
  , phone :: String
  , setDefault :: Boolean
  }