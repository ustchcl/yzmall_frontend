module Yzmall.Data.BaseTypes where



type AddressBase row = 
  ( name :: String
  , address :: String
  , phone :: String
  | row
  )


type IDCard = 
  { name :: String
  , idCard :: String
  }

type LoginFields = 
  { phone :: String
  , password :: String 
  }