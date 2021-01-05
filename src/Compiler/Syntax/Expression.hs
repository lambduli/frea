{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Compiler.Syntax.Expression where

import Compiler.Syntax (Lit, Type, MatchGroup)
import Data.List


data Expression
  = Var String
  | Op String
  | Con String
  -- | DataValue String [Expression] -- Tag + Arguments -- only runtime value, user can't write it
  | Lit Lit
  | Lam String Expression
  | App Expression Expression
  | Tuple [Expression]
  | NegApp Expression -- syntactic negation -- think about it
  | MatchWith Expression MatchGroup -- OK
  | If Expression Expression Expression -- OK
  | Let String Expression Expression -- OK
  | Typed Type Expression -- OK

instance Show Expression where
  show (Var name) = name
  show (Op op) = op
  show (Con name) = name
  show (Lit lit) = show lit
  show (Lam arg body) = "(lambda " ++ arg ++ " -> " ++ show body ++ ")"
  show (App left right) = "(" ++ show left ++ " " ++ show right ++ ")"
  show (Tuple exprs) = "(" ++ intercalate ", " (map show exprs) ++ ")"
  show (NegApp exp) = "-" ++ show exp
  show (If cond' then' else') = "(if " ++ show cond' ++ " then " ++ show then' ++ " else " ++ show else' ++ ")"
  show (Let name value expr) = "(let " ++ show name ++ " = " ++ show value ++ " in " ++ show expr ++ ")"
  -- typed
  -- matchwith
