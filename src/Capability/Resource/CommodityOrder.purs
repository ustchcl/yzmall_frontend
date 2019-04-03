module Yzmall.Api.Capablity.Resource.CommodityOrder where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Slug (Slug)
import Yzmalal.Data.CommodityOrder (CommodityOrder, PayForOrderResult)
import Yzmall.Data.ACTSell (ACTSell, MYTSharedRecord)


class Monad m <= ManageOrder m where
  viewOrder :: m (Maybe (Array CommodityOrder))
  payForOrder :: Slug -> Maybe String -> m (Maybe PayForOrderResult)
  createOrderSpecial :: Slug -> CreateOrderParams -> m (Maybe CommodityOrder)
  payForOrderSpecial :: Slug -> Maybe String -> m (Maybe PayForOrderResult)
  deleteOrder :: Slug -> m Unit
  createACTSellCommission :: String -> m (Maybe ACTSell)
  createMYTSellRush :: m (Maybe ACTSell)
  createACTSellRebate :: String -> m (Maybe ACTSell)
  createOrder :: Slug -> CreateOrderParams -> m (Maybe CommodityOrder)
  mytSharedRecord :: m (Maybe MYTSharedRecord)
  getACTSells :: m (Maybe (Array ACTSell))

instance manageOrderHalogenM :: ManageOrder m => ManageOrder (HalogenM s f g p o m) where
  viewOrder = lift  viewOrder
  payForOrder s = lift <<< payForOrder s
  createOrderSpecial slug = lift <<< createOrderSpecial slug
  payForOrderSpecial s = lift <<< payForOrderSpecial s
  deleteOrder = lift <<< deleteOrder
  createACTSellCommission = lift <<< createACTSellCommission
  createACTSellRebate = lift <<<  createACTSellRebate
  createMYTSellRush = lift createMYTSellRush
  createOrder slug = lift <<< createOrder slug
  mytSharedRecord = lift mytSharedRecord
  getACTSells = lift getACTSells

type CreateOrderParams =
  { tag :: Int
  , addressId :: Maybe Int
  , amount :: Int
  }