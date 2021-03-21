{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Compiler.Syntax.Expression where

import Compiler.Syntax (Lit, Type, MatchGroup)
import Data.List


data Expression
  = Var String -- OK
  | Op String -- OK
  | Lit Lit -- OK
  | Lam String Expression -- OK
  | App Expression Expression -- OK
  | Tuple [Expression] -- OK
  | List [Expression] -- OK
  | If Expression Expression Expression -- OK
  | Let String Expression Expression -- OK
  | Fix Expression -- OK
  | Intro String [Expression]
  | Elim String [Expression] -- TODO: revisit
  -- | Typed Type Expression -- OK
  -- | MatchWith Expression MatchGroup -- OK
  deriving (Eq)
  
instance Show Expression where
  show (Var name) = name
  show (Op op) = op
  show (Lit lit) = show lit
  show (Lam arg body) = "(\\ " ++ arg ++ " -> " ++ show body ++ ")"
  show (App left right) = "(" ++ show left ++ " " ++ show right ++ ")"
  show (Tuple exprs) = "(" ++ intercalate ", " (map show exprs) ++ ")"
  show (List exprs) = "[" ++ intercalate ", " (map show exprs) ++ "]"
  show (If cond' then' else') = "if " ++ show cond' ++ " then " ++ show then' ++ " else " ++ show else'
  show (Let name value expr) = "let " ++ name ++ " = " ++ show value ++ " in " ++ show expr
  show (Fix expr) = "fix " ++ show expr
  show (Intro name exprs) = "(" ++ name ++ intercalate ", " (map show exprs) ++ ")"
  show (Elim name exprs) = "<not implemented yet>"
  -- typed
  -- matchwith
