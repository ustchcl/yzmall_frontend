let configFields =
[ "commissionRatio"
, "netCostLimitPerAccount"
, "rebateRatio_1"
, "rebateRatio_2"
, "rebateRatio_3"
, "rebateRatio_ex"
, "sellACTLowerLimit"
, "sellACTUpperLimit"
, "sellFacilityRatio_1"
, "sellFacilityRatio_2"
, "sellFacilityRatio_3"
, "sellFacilityRatio_ex"
]

let configNames = 
[ "净消费上限"
, "ACT数量下限"
, "ACT数量上限"
, "返利系数-VIP"
, "返利系数-总监"
, "返利系数-总裁"
, "额外返利系数"
, "出售额度系数-VIP"
, "出售额度系数-总监"
, "出售额度系数-总裁"
, "额外出售额度系数"
, "代售系数"
]
 
/*************************** */
 let accountFields =
 [
    "inviter",
    "nickname",
    "phone",
    "role",
    "grade",
    "name",
    "idCard",
    "gold",
    "regularCommodityCost",
    "specialCommodityCost",
    "commissionBalance",
    "commissionSellFacility",
    "commissionSell",
    "rebateBalance",
    "rebateSell",
    "defaultAddress",
    "defaultBankCard"
 ]

let accountNames =
 [
    "邀请者",
    "昵称",
    "手机号",
    "角色",
    "等级",
    "姓名",
    "身份证",
    "金币数",
    "正价商品消费金额(￥)",
    "特价商品消费金额(￥)",
    "可出售的代售金额(MYT)",
    "代售金额出售额度(MYT)",
    "已出售的代售金额(MYT)",
    "可出售的返利金额(MYT)",
    "已出售的返利金额(MYT)",
    "默认收货地址的Id",
    "默认使用的银行卡Id"
 ]

/********************** */

let commodityFields = 
[
    "name",
    "category",
    "price",
    "primeCost",
    "rebateACT",
    "gold",
    "stock",
    "sale",
    "onSale",
    "recommend",
    "thumbnail",
    "picture",
    "tag"
]

let commodityNames = 
[
    "商品的名称",
    "商品的类别 REGULAR/SPECIAL",
    "商品的价格",
    "商品的成本价",
    "MYT基准值",
    "金币数量",
    "库存数量",
    "售出数量",
    "是否上架",
    "是否为推荐商品",
    "该商品的缩略图",
    "该商品的详细图",
    "可选择标签 以英文逗号分割"
]

/*********** */

let orderFields =
[
    "accountId",
    "commodityId",
    "tag",
    "amount",
    "commissionCost",
    "rebateCost",
    "payCost",
    "addressId",
    "process",
    "payPass",
    "payId",
    "expressCompany",
    "expressId",
    "createTime",
    "payTime",
    "deliverTime",
    "refundTime"
]

let orderNames =
[
    "创建该订单的AccountID",
    "该订单的商品ID",
    "该商品的由用户选择的标签",
    "该订单的商品数量",
    "使用commissionBalance支付的货款",
    "使用rebateBalance支付的货款",
    "用人民币支付的货款",
    "该订单的收货地址",
    "订单的进度",
    "支付通道的id",
    "支付流水号",
    "快递公司的id",
    "快递单号",
    "订单创建的时间",
    "订单支付成功的时间",
    "订单发货的时间",
    "订单退款成功的时间"
]

/****** */
let actSharedRecordFields = [
    "price",
    "remainPurchase"
]

let actSharedRecordNames = [
    "出售1个MYT能获得的人民币",
    "剩余可收购的MYT数量"
]