module Compiler.Syntax.Type where

import Data.List (intercalate)

-- I am gonna need to implement AST FIX similar to the one in the Transform
-- I should probably move the logic from the JS Syntax tree to Gliss AST
-- and also add a fix, which will transform TyVar such as Int or Bool (if it's declared by user)
-- to TyCon
--  
data Type
  = TyVar String
  | TyCon String
  | TyTuple [Type]
  | TyList Type
  -- | BuiltInTyCon BuiltInTyCon
  -- | AppTy Type Type
  | TyArr Type Type
  -- | Forall String Type -- type level lambda

instance Show Type where
  show (TyVar name)
    = name
  show (TyCon name)
    = name
  show (TyTuple types)
    = "(" ++ intercalate ", " (map show types) ++ ")"
  show (TyList type')
    = "[" ++ show type' ++ "]"
  show (TyArr left@(TyArr _ _) res'type)
    = "(" ++ show left ++ ") -> " ++ show res'type
  show (TyArr arg'type res'type)
    = show arg'type ++ " -> " ++ show res'type


data Scheme
  = ForAll [String] Type

instance Show Scheme where
  show (ForAll [] type')
    = show type'
  show (ForAll type'args type')
    = "forall " ++ intercalate " " type'args ++ " . " ++ show type'

-- data BuiltInTyCon
--   = UnitTyCon
--   | FunTyCon