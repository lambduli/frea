{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Compiler.Syntax.Expression where

import Compiler.Syntax (Lit, Type, MatchGroup)
import Compiler.Syntax.Declaration
import Data.List


data Expression
  = Var String
  | Op String
  | Lit Lit
  | Lam String Expression
  | App Expression Expression
  | Tuple [Expression]
  | If Expression Expression Expression
  | Let [(String, Expression)] Expression
  -- | Fix Expression
  | Ann Type Expression
  | Intro String [Expression]
  | Elim [ConstrDecl] Expression [Expression]
  --     for matching elim what? destructors
  deriving (Eq)
  -- | MatchWith Expression MatchGroup -- OK


instance Show Expression where
  show (Var name) = name
  show (Op op) = op
  show (Lit lit) = show lit
  show (Lam arg body) = "(\\ " ++ arg ++ " -> " ++ show body ++ ")"
  show (App left right) = "(" ++ show left ++ " " ++ show right ++ ")"
  show (Tuple exprs) = "(" ++ intercalate ", " (map show exprs) ++ ")"
  show (If cond' then' else') = "if " ++ show cond' ++ " then " ++ show then' ++ " else " ++ show else'
  show (Let pairs expr) = "let " ++ intercalate "\n" (map (\ (name, val) -> name ++ show val) pairs) ++ " in " ++ show expr
  -- show (Fix expr) = "fix " ++ show expr
  show (Ann type' expr) = show expr ++ " :: " ++ show type'
  show (Intro name exprs) = "(" ++ name ++ " " ++ intercalate " " (map show exprs) ++ ")"
  show (Elim constrs val'to'elim destrs) = "<eliminator>"
  -- matchwith
