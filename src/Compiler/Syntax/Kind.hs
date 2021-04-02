module Compiler.Syntax.Kind where


data Kind
  = Star
  | KArr Kind Kind
  | KVar String -- kind meta variable
  deriving (Eq)


instance Show Kind where
  show Star = "*"
  show (KArr k'dom@(KArr _ _) k'codom) = "(" ++ show k'dom ++ ") -> " ++ show k'codom
  show (KArr k'dom k'codom) = show k'dom ++ " -> " ++ show k'codom
  show (KVar name) = name ++ "?"
