/* tslint:disable */
// Generated using typescript-generator version 2.5.423 on 2019-03-06 17:40:32.

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

export type Role = "ADMIN" | "CUSTOMER" | "SUPPLIER";