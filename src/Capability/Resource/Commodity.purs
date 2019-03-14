module Yzmall.Resource.Commodity where
import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Slug (Slug)
import Yzmall.Api.Endpoint (CommodityParams)
import Yzmall.Data.Commodity (Commodity)

class Monad m <= ManageCommodity m where
  getCommodities :: CommodityParams -> m (Maybe (Array Commodity))
  getCommodity :: Slug -> m (Maybe Commodity)

instance manageCommodityHalogenM :: ManageCommodity m => ManageCommodity (HalogenM s f g p o m) where
  getCommodities = lift <<< getCommodities
  getCommodity = lift <<< getCommodity