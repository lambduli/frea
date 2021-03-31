module Compiler.Syntax.Kind where


data Kind
  = Star
  | KArr Kind Kind
  deriving (Eq)


instance Show Kind where
  show Star = "*"
  show (KArr k'dom k'codom) = show k'dom ++ " -> " ++ show k'codom
