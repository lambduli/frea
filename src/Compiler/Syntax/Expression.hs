{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Compiler.Syntax.Expression where

import Compiler.Syntax (Lit, Type, MatchGroup)
import Data.List


data Expression
  = Var String -- OK
  | Op String -- OK
  | Con String -- OK
  -- | DataValue String [Expression] -- Tag + Arguments -- only runtime value, user can't write it
  | Lit Lit -- OK
  | Lam String Expression -- OK
  | App Expression Expression -- OK
  | Tuple [Expression] -- OK
  | List [Expression] -- OK
  -- | NegApp Expression -- syntactic negation -- think about it
  | If Expression Expression Expression -- OK
  | Let String Expression Expression -- OK
  | Fix Expression -- OK
  -- | Typed Type Expression -- OK
  -- | MatchWith Expression MatchGroup -- OK
  
instance Show Expression where
  show (Var name) = name
  show (Op op) = op
  show (Con name) = name
  show (Lit lit) = show lit
  show (Lam arg body) = "(\\ " ++ arg ++ " -> " ++ show body ++ ")"
  show (App left right) = "(" ++ show left ++ " " ++ show right ++ ")"
  show (Tuple exprs) = "(" ++ intercalate ", " (map show exprs) ++ ")"
  show (List exprs) = "[" ++ intercalate ", " (map show exprs) ++ "]"
  -- show (NegApp exp) = "-" ++ show exp
  show (If cond' then' else') = "if " ++ show cond' ++ " then " ++ show then' ++ " else " ++ show else'
  show (Let name value expr) = "let " ++ name ++ " = " ++ show value ++ " in " ++ show expr
  show (Fix expr) = "fix " ++ show expr
  -- typed
  -- matchwith
