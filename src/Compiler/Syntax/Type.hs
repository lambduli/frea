module Compiler.Syntax.Type where

import Data.List (intercalate)


data Type
  = TyVar String
  | TyCon String
  | TyTuple [Type]
  | TyList Type
  | TyArr Type Type
  | TyApp Type Type
  deriving (Eq)


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
  show (TyApp t'left t'right)
    = "(" ++ show t'left ++ " " ++ show t'right ++ ")"


data Scheme
  = ForAll [String] Type
  deriving (Eq)

instance Show Scheme where
  show (ForAll [] type')
    = show type'
  show (ForAll type'args type')
    = "forall " ++ intercalate " " type'args ++ " . " ++ show type'
