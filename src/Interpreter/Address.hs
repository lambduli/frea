module Interpreter.Address where


newtype Address = Addr Int
  deriving (Show, Ord, Eq)