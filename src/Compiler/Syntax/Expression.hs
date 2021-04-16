{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Compiler.Syntax.Expression where

import Compiler.Syntax (Lit, Type, MatchGroup)
import Compiler.Syntax.Declaration
import Data.List


data Expression
  = Var String -- OK
  | Op String -- OK
  | Lit Lit -- OK
  | Lam String Expression -- OK
  | App Expression Expression -- OK
  | Tuple [Expression] -- OK
  | If Expression Expression Expression -- OK
  | Let String Expression Expression -- OK
  | Fix Expression -- OK
  | Ann Type Expression
  | Intro String [Expression]
  | Elim [ConstrDecl] Expression [Expression]
  --     for matching elim what? destructors
  deriving (Eq)

  -- | Typed Type Expression -- OK
  -- | MatchWith Expression MatchGroup -- OK
  
instance Show Expression where
  show (Var name) = name
  show (Op op) = op
  show (Lit lit) = show lit
  show (Lam arg body) = "(\\ " ++ arg ++ " -> " ++ show body ++ ")"
  show (App left right) = "(" ++ show left ++ " " ++ show right ++ ")"
  show (Tuple exprs) = "(" ++ intercalate ", " (map show exprs) ++ ")"
  show (If cond' then' else') = "if " ++ show cond' ++ " then " ++ show then' ++ " else " ++ show else'
  show (Let name value expr) = "let " ++ name ++ " = " ++ show value ++ " in " ++ show expr
  show (Fix expr) = "fix " ++ show expr
  show (Intro name exprs) = "(" ++ name ++ " " ++ intercalate " " (map show exprs) ++ ")"
  show (Elim constrs val'to'elim destrs) = "<eliminator>"
  -- typed
  -- matchwith
