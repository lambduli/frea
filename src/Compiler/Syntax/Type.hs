module Compiler.Syntax.Type where

import Data.List (intercalate)

import Compiler.Syntax.Kind (Kind(..))


type Name = String


{-
    Now there's the question:
    Do I add TyVar and TyCon types?
-}

data TVar = TVar Name Kind
  deriving (Eq)


data TCon = TCon Name Kind
  deriving (Eq)


data Type
  = TyVar TVar
  | TyCon TCon
  | TyTuple [Type] -- TODO: remove it too?
  | TyArr Type Type -- TODO: remove it and represent it as an application instead
  | TyApp Type Type

  | TyOp String Type -- type operator/function/alias
  deriving (Eq)


{- Show instance is just for quick'n'dirty serialization purpose. -}
instance Show Type where
  show (TyVar (TVar name kind'))
    = name -- ignoring the kind of the type variable
  show (TyCon (TCon name kind'))
    = name -- ignoring the kind of the type constant
  show (TyTuple types)
    = "(" ++ intercalate ", " (map show types) ++ ")"
  show (TyArr left@(TyArr _ _) res'type)
    = "(" ++ show left ++ ") -> " ++ show res'type
  show (TyArr arg'type res'type)
    = show arg'type ++ " -> " ++ show res'type
  show (TyApp t'left t'right@(TyApp _ _))
    = show t'left ++ " (" ++ show t'right ++ ")"
  show (TyApp t'left t'right)
    = show t'left ++ " " ++ show t'right

  show (TyOp var type')
    = "(λ " ++ var ++ " . " ++ show type' ++ ")"


data Scheme
  = ForAll [String] Type
  deriving (Eq)

instance Show Scheme where
  show (ForAll [] type')
    = show type'
  show (ForAll type'args type')
    = "forall " ++ intercalate " " type'args ++ " . " ++ show type'


class HasKind t where
  kind :: t -> Kind


instance HasKind TVar where
  kind (TVar _ k) = k


instance HasKind TCon where
  kind (TCon _ k) = k


{- following definition is only partial function, it is assumed that the types are always well formed -}
instance HasKind Type where
  kind (TyVar tv) = kind tv
  kind (TyCon tcon) = kind tcon
  kind (TyTuple _) = Star -- assuming the type is well formed
  kind (TyArr _ _) = Star -- assuming the type is well formed
  kind (TyApp t _)
    = case kind t of
      KArr _ k -> k
      -- assuming the type is well formed, therefore there's no other option
