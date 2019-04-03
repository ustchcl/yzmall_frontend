let target = 
`
id: number;
accountId: number;
name: string;
cardId: string;
region: string;
phone: string;
bank: string;
`

function main () {
  let arr = target.split(";");
  let result = []
  arr.forEach(ele => {
    let index = ele.indexOf(":");
    if (index == -1) {
      return;
    } else {
      let key = ele.slice(1, index);
      result.push(key);
      console.log(`${key} <- obj .: "${key}"`)
    }
  });
  console.log(`pure { ${result.join(', ')} }`)
}

main();


let moduleName = "Order"
let apis = 
`
viewOrder :: m (Maybe (Array CommodityOrder))
  payForOrder :: Slug -> m (Maybe CommodityOrder)
  createOrderSpecial :: CreateOrderParams -> m (Maybe CommodityOrder)
  payForOrderSpecial :: Slug -> m (Maybe CommodityOrder)
  deleteOrder :: Slug -> m Unit
  createACTSellCommission :: m (Maybe ACTSell)
  createACTSellRebate :: m (Maybe ACTSell)
  createOrder :: CreateOrderParams -> m (Maybe CommodityOrder)
`

function lift() {
  console.log(`instance manage${moduleName}HalogenM :: Manage${moduleName} m => Manage${moduleName} (HalogenM s f g p o m) where`);
  let arr = apis.split("\n");
  let result = []
  arr.forEach(ele => {
    let index = ele.indexOf("::");
    if (index == -1) {
      return;
    } else {
      let after = ele.indexOf("->") != -1 ? "<<<" : "";
      let key = ele.slice(0, index).trim();
      console.log(`${key} = lift ${after} ${key}`)
    }
  });
}

// lift();