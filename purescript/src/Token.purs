module Token where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Token
  = Illegal
  | Eof
  | Ident String
  | Int Number
  | Equal
  | Plus
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LSquirly
  | RSquirly
  | Function
  | Let

derive instance genericToken :: Generic Token _

instance showToken :: Show Token where
  show = genericShow

instance eqToken :: Eq Token where
  eq = genericEq
