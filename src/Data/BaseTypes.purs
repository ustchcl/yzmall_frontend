module Yzmall.Data.BaseTypes where


type BankCardBase row = 
  ( name :: String
  , cardId :: String
  , region :: String
  , phone :: String
  , bank :: String 
  | row
  )

type BankCard = { | BankCardBase (id :: Int, accountId :: Int) }
type BankCardFields = { | BankCardBase (setDefault :: Boolean) }

type AddressBase row = 
  ( name :: String
  , address :: String
  , phone :: String
  | row
  )

type Address = { | AddressBase (id :: Int, accountId :: Int) }
type AddressFields = { | AddressBase (setDefault :: Boolean)}


type IDCard = 
  { name :: String
  , idCard :: String
  }

type LoginFields = 
  { phone :: String
  , password :: String 
  }