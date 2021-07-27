module Compiler.Syntax.Type where

import Data.List (intercalate)

import Compiler.Syntax.Kind (Kind(..))


type Name = String


{-
    Now there's the question:
    Do I add TyVar and TyCon types?
-}

data TVar = TVar Name Kind
  -- deriving (Eq)

instance Eq TVar where
  (TVar name'l kind'l) == (TVar name'r kind'r) = name'l == name'r
-- TODO: FIX LATER: I am trying this because of the inconsistency in the AST after it's parsed
-- every occurence of the type variable is given a fresh kind variable, this means they are considered different things
-- that can be mitigated by not taking the kind in account when comparing


instance Show TVar where
  show (TVar name kind) = "(" ++ name ++ " :: " ++ show kind ++ ")"


{- NOTE: this is SOLELY because Map TVar _ and it's `Map.union` operation -}
instance Ord TVar where
  (TVar n'l _) <= (TVar n'r _) = n'l <= n'r


data TCon = TCon Name Kind
  -- deriving (Eq)

instance Eq TCon where
  (TCon name'l kind'l) == (TCon name'r kind'r) = name'l == name'r
-- TODO: FIX LATER: I am trying this because of the inconsistency in the AST after it's parsed
-- every occurence of the type constant is given a fresh kind variable, this means they are considered different things
-- that can be mitigated by not taking the kind in account when comparing


instance Show TCon where
  show (TCon name kind) = "(" ++ name ++ " :: " ++ show kind ++ ")"


data Type
  = TyVar TVar
  | TyCon TCon
  | TyTuple [Type] -- TODO: remove it too?
  | TyApp Type Type

  | TyOp String Type -- type operator/function/alias
  | TySyn [TVar] Type -- this should replace the TyOp
  -- it will only work for fully applied type synonyms
  -- TODO: comment out TyOp and implement TySyn instead
  deriving (Eq)


{- Show instance is just for quick'n'dirty serialization purpose. -}
instance Show Type where
  show (TyVar (TVar name kind'))
    = name -- ignoring the kind of the type variable
  show (TyCon (TCon name kind'))
    = "(" ++ name ++ " :: " ++ show kind' ++ ")" -- ignoring the kind of the type constant
  show (TyTuple types)
    = "(" ++ intercalate ", " (map show types) ++ ")"
  -- show (TyArr left@(TyArr _ _) res'type)
    -- = "(" ++ show left ++ ") -> " ++ show res'type
  -- show (TyArr arg'type res'type)
    -- = show arg'type ++ " -> " ++ show res'type
  show (TyApp t'left t'right@(TyApp _ _))
    = show t'left ++ " (" ++ show t'right ++ ")"
  show (TyApp t'left t'right)
    = show t'left ++ " " ++ show t'right

  show (TyOp var type')
    = "(λ " ++ var ++ " . " ++ show type' ++ ")"


data Scheme
  = ForAll [TVar] Type
  deriving (Eq)

instance Show Scheme where
  show (ForAll [] type')
    = show type'
  show (ForAll type'args type')
    = "forall " ++ unwords (map show type'args) ++ " . " ++ show type'


class HasKind t where
  kind :: t -> Kind
  -- NOTE: maybe make it return (Maybe Kind)


instance HasKind TVar where
  kind (TVar _ k) = k


instance HasKind TCon where
  kind (TCon _ k) = k


{- following definition is only partial function, it is assumed that the types are always well formed -}
instance HasKind Type where
  kind (TyVar tv) = kind tv
  kind (TyCon tcon) = kind tcon
  kind (TyTuple _) = Star -- assuming the type is well formed
  -- kind (TyArr _ _) = Star -- assuming the type is well formed
  kind (TyApp t _)
    = case kind t of
      KArr _ k -> k
      -- assuming the type is well formed, therefore there's no other option



data Qualified t = [Predicate] :=> t
  deriving (Eq)


data Predicate = IsIn Name Type
  deriving (Eq)

