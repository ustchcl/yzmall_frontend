/* tslint:disable */
// Generated using typescript-generator version 2.5.423 on 2019-03-17 22:57:04.

export interface Account extends Serializable, UserDetails {
    id: number;
    inviter: number;
    nickname: string;
    phone: string;
    role: Role;
    grade: number;
    name: string;
    idCard: string;
    gold: number;
    regularCommodityCost: number;
    specialCommodityCost: number;
    commissionBalance: number;
    commissionSellFacility: number;
    commissionSell: number;
    rebateBalance: number;
    rebateSell: number;
    defaultAddress: number;
    defaultBankCard: number;
}

export interface Address extends Serializable {
    id: number;
    accountId: number;
    name: string;
    address: string;
    phone: string;
}

export interface BankCard extends Serializable {
    id: number;
    accountId: number;
    name: string;
    cardId: string;
    region: string;
    phone: string;
    bank: string;
    huijuSignId: string;
}

export interface ACTSell extends Serializable {
    id: number;
    accountId: number;
    category: ACTSellCategory;
    price: number;
    amount: number;
    process: ACTSellProcess;
    payPass: number;
    payId: string;
    createTime: Date;
    soldTime: Date;
    rejectTime: Date;
}

export interface ACTSharedRecord extends Serializable, LongId {
    price: number;
    remainPurchase: number;
}

export interface Commission extends Serializable {
    id: number;
    accountId: number;
    commodityId: number;
    tag: number;
    amount: number;
    process: CommissionProcess;
    expressCompany: number;
    expressId: string;
    createTime: Date;
    soldTime: Date;
    deliverTime: Date;
}

export interface Commodity extends Serializable {
    id: number;
    name: string;
    category: CommodityCategory;
    price: number;
    primeCost: number;
    rebateMYT: number;
    gold: number;
    stock: number;
    sale: number;
    onSale: boolean;
    recommend: boolean;
    thumbnail: string;
    picture: string;
    tag: Tag[];
}

export interface CommodityOrder extends Serializable {
    id: number;
    accountId: number;
    commodityId: number;
    tag: number;
    amount: number;
    commissionCost: number;
    rebateCost: number;
    payCost: number;
    addressId: number;
    process: OrderProcess;
    payPass: number;
    payId: string;
    expressCompany: number;
    expressId: string;
    createTime: Date;
    payTime: Date;
    deliverTime: Date;
    refundTime: Date;
}

export interface MYTSell extends Serializable {
    id: number;
    accountId: number;
    category: MYTSellCategory;
    price: number;
    amount: number;
    process: MYTSellProcess;
    payPass: number;
    payId: string;
    createTime: Date;
    soldTime: Date;
    rejectTime: Date;
}

export interface MYTSharedRecord extends Serializable, LongId {
    price: number;
    remainPurchase: number;
}

export interface PayForOrderResult {
    expectVcode: boolean;
    order: CommodityOrder;
}

export interface Tag extends Serializable {
    id: number;
    content: string;
}

export interface HeadPicture extends Serializable {
    id: number;
    picture: string;
    commodityId: number;
}

export interface Notice extends Serializable {
    id: number;
    content: string;
}

export interface Config extends Serializable, LongId {
    netCostLimitPerAccount: number;
    sellMYTLowerLimit: number;
    sellMYTUpperLimit: number;
    rebateRatio_1: number;
    rebateRatio_2: number;
    rebateRatio_3: number;
    rebateRatio_ex: number;
    sellFacilityRatio_1: number;
    sellFacilityRatio_2: number;
    sellFacilityRatio_3: number;
    sellFacilityRatio_ex: number;
    commissionRatio: number;
}

export interface GrantedAuthority extends Serializable {
    authority: string;
}

export interface Serializable {
}

export interface UserDetails extends Serializable {
    authorities: GrantedAuthority[];
    accountNonLocked: boolean;
    accountNonExpired: boolean;
    credentialsNonExpired: boolean;
    enabled: boolean;
    username: string;
    password: string;
}

export interface LongId {
    id: number;
}

export type Role = "ADMIN" | "CUSTOMER" | "SUPPLIER";

export type ACTSellCategory = "COMMISSION" | "REBATE";

export type ACTSellProcess = "WAIT_FOR_REVIEW" | "WAIT_FOR_PAYMENT_RESULT" | "SOLD" | "REJECT";

export type CommissionProcess = "WAIT_FOR_REVIEW" | "ON_SALE" | "SOLD" | "DELIVERED";

export type CommodityCategory = "REGULAR" | "SPECIAL";

export type OrderProcess = "WAIT_FOR_PAYMENT" | "WAIT_FOR_PAYMENT_RESULT" | "WAIT_FOR_DELIVER" | "DELIVERED" | "WAIT_FOR_REFUND" | "REFUNDED";

export type MYTSellCategory = "COMMISSION" | "REBATE";

export type MYTSellProcess = "WAIT_FOR_REVIEW" | "WAIT_FOR_PAYMENT_RESULT" | "SOLD" | "REJECT";