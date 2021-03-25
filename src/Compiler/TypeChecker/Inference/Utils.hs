{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler.TypeChecker.Inference.Utils where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Bifunctor (second)
import Data.List
import Data.Functor.Identity

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Compiler.Syntax.Declaration
import Compiler.TypeChecker.TypeError
import Compiler.TypeChecker.Type


import Compiler.Syntax
  ( Bind(..)
  , Declaration(..), ConstrDecl(..)
  , Expression(..)
  , Lit(..)
  , MatchGroup(..), Match(..)
  , Pattern(..)
  , Sig(..)
  , Type(..), Scheme(..))


-- Inference monad
type Infer a
  = ReaderT
      TypeEnv           -- Typing environment
      (StateT           -- Inference state
        InferState
        (Except         -- Inference errors
          TypeError))
      a                 -- Result


newtype TypeEnv = Env (Map.Map String Scheme)
  deriving (Show)


type Constraint = (Type, Type)


-- Inference state
newtype InferState
  = InferState { count :: Int }


-- Constraint solver monad
type Solve a = ExceptT TypeError Identity a

type Unifier = (Subst, [Constraint])


-- type substitution -- ordered mapping between name and type
newtype Subst = Sub (Map.Map String Type)
  deriving (Eq, Show)


-- initial inference state
init'infer :: InferState
init'infer = InferState { count = 0 }


unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return empty'subst
unifies (TyVar v) t = v `bind` t
unifies t (TyVar v) = v `bind` t
unifies (TyArr t1 t2) (TyArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TyCon name'l) (TyCon name'r)
  | name'l == name'r = return empty'subst
  | otherwise = throwError $ UnifMismatch name'l name'r

unifies (TyTuple ts'left) (TyTuple ts'right)
  = if length ts'left /= length ts'right
    then throwError $ UnifShapeMismatch (TyTuple ts'left) (TyTuple ts'right)
    else unifyMany ts'left ts'right
    -- else foldM
    --   (\ subst' (t'left, t'right) -> do
    --     subst'new <- unify (apply subst' t'left) (apply subst' t'right)
    --     return (subst' `compose'subst` subst'new))
    --   empty'subst
    --   (zip ts'left ts'right)

unifies (TyList t'left) (TyList t'right)
  = unifies t'left t'right

-- unifies (TyArr left'a right'a) (TyArr left'b right'b) = do
--   subst'left <- unifies left'a left'b
--   subst'right <- unifies (apply subst'left right'a) (apply subst'left right'b)
--   return (subst'right `compose'subst` subst'left)



unifies t1 t2 = throwError $ UnifShapeMismatch t1 t2


unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return empty'subst
unifyMany (t'l : ts'l) (t'r : ts'r) = do
  su1 <- unifies t'l t'r
  su2 <- unifyMany (apply su1 ts'l) (apply su1 ts'r)
  return (su2 `compose'subst` su1)
unifyMany t'l t'r = throwError $ UnifCountMismatch t'l t'r


-- Unification solver
solver :: Unifier -> Solve Subst
solver (subst, constraints) =
  case constraints of
    [] -> return subst
    ((type'l, type'r) : constrs) -> do
      subst'  <- unifies type'l type'r
      solver (subst' `compose'subst` subst, apply subst' constrs)


empty'unifier :: Unifier
empty'unifier = (empty'subst, [])





class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set String

instance Substitutable Type where
  apply (Sub s) var@(TyVar varname)
    = Map.findWithDefault var varname s
  apply _ (TyCon conname)
    = TyCon conname
  apply s (left `TyArr` right)
    = apply s left `TyArr` apply s right
  apply s (TyTuple types)
    = TyTuple $ map (apply s) types
  apply s (TyList type')
    = TyList $ apply s type'

  ftv type' = case type' of
    TyVar name -> Set.singleton name
    TyCon name -> Set.empty
    TyTuple ts -> foldl (\ set' t' -> Set.union set' (ftv t')) Set.empty ts
    TyList t -> ftv t
    TyArr left right -> ftv left `Set.union` ftv right


instance Substitutable Scheme where
  apply (Sub s) (ForAll varnames type')
    = ForAll varnames $ apply s' type'
      where s' = Sub $ foldr Map.delete s varnames

  ftv (ForAll vars type')
    = ftv type' `Set.difference` Set.fromList vars


instance Substitutable Constraint where
  apply s (t'l, t'r)
    = (apply s t'l, apply s t'r)

  ftv (t'l, t'r)
    = ftv t'l `Set.union` ftv t'r


instance Substitutable a => Substitutable [a] where
  apply = map . apply
  ftv   = foldr (Set.union . ftv) Set.empty


instance Substitutable TypeEnv where
  apply subst (Env type'env)
    = Env $ Map.map
        (apply subst)
        type'env

  ftv (Env type'env)
    = Map.foldr
        (\ scheme free'set -> free'set `Set.union` ftv scheme)
        Set.empty
        type'env






runInfer :: TypeEnv -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) init'infer










extend :: TypeEnv -> (String, Scheme) -> TypeEnv
extend (Env env) (ty'var, scheme) = Env $ Map.insert ty'var scheme env


remove :: TypeEnv -> String -> TypeEnv
remove (Env env) var = Env (Map.delete var env)


-- Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve constrs = runIdentity $ runExceptT $ solver state
  where state = (empty'subst, constrs)





closeOver :: Type -> Scheme
closeOver = normalize . generalize (Env Map.empty)


-- closeOver :: (Subst, Type) -> Scheme
-- closeOver (subst, type')
--   = normalize sc
  -- where sc = generalize (Env Map.empty) (apply subst type')


normalize :: Scheme -> Scheme
normalize (ForAll type'args body) = ForAll (fmap snd ord) (normtype body)
  where
    ord = zip (Set.toList . ftv $ body) letters

    normtype (TyArr a b) = TyArr (normtype a) (normtype b)
    normtype (TyCon a) = TyCon a
    normtype (TyTuple ts) = TyTuple $ map normtype ts
    normtype (TyList t) = TyList $ normtype t
    normtype (TyVar a) =
      case lookup a ord of
        Just x -> TyVar x
        Nothing -> error $ "Type variable " ++ show a ++ " not in the signature."


empty'subst :: Subst
empty'subst = Sub Map.empty


-- not really empty
empty'env :: TypeEnv
empty'env = Env $ Map.fromList
  [ ("#fst",  ForAll ["a", "b"] (TyArr (TyTuple [TyVar "a", TyVar "b"]) (TyVar "a")))
  , ("#snd",  ForAll ["a", "b"] (TyArr (TyTuple [TyVar "a", TyVar "b"]) (TyVar "b")))

  , ("#&&",   ForAll ["a"]      (TyTuple [t'Bool, t'Bool] `TyArr` t'Bool))
  , ("#||",   ForAll ["a"]      (TyTuple [t'Bool, t'Bool] `TyArr` t'Bool))
  
  , ("#=",    ForAll ["a"]      (TyTuple [TyVar "a", TyVar "a"] `TyArr` t'Bool))
  , ("#<",    ForAll ["a"]      (TyTuple [TyVar "a", TyVar "a"] `TyArr` t'Bool))
  , ("#>",    ForAll ["a"]      (TyTuple [TyVar "a", TyVar "a"] `TyArr` t'Bool))

  , ("#+",    ForAll []         (TyTuple [t'Int, t'Int] `TyArr` t'Int))
  , ("#+.",    ForAll []         (TyTuple [t'Double, t'Double] `TyArr` t'Double))

  , ("#*",    ForAll []         (TyTuple [t'Int, t'Int] `TyArr` t'Int))
  , ("#*.",    ForAll []         (TyTuple [t'Double, t'Double] `TyArr` t'Double))

  , ("#-",    ForAll []         (TyTuple [t'Int, t'Int] `TyArr` t'Int))
  , ("#-.",    ForAll []         (TyTuple [t'Double, t'Double] `TyArr` t'Double))

  , ("#div",  ForAll []         (TyTuple [t'Int, t'Int] `TyArr` t'Int))
  , ("#/",    ForAll []         (TyTuple [t'Double, t'Double] `TyArr` t'Double))
  , ("#++",   ForAll ["a"]      (TyTuple [TyList (TyVar "a"), TyList (TyVar "a")] `TyArr` TyList (TyVar "a")))
  -- prepend element to a list
  , ("#:",    ForAll ["a"]      (TyTuple [TyVar "a", TyList (TyVar "a")] `TyArr` TyList (TyVar "a")))
  , ("#head", ForAll ["a"]      (TyList (TyVar "a") `TyArr` TyVar "a"))
  , ("#tail", ForAll ["a"]      (TyList (TyVar "a") `TyArr` TyList (TyVar "a")))
  , ("#nil?", ForAll ["a"]      (TyList (TyVar "a") `TyArr` t'Bool))
  ]


-- compose'subst :: Subst -> Subst -> Subst
-- compose'subst (Sub subst'left) subst'right
--   = let (Sub subst'right') = apply (Sub subst'left) subst'right
--     in Sub $ subst'left `Map.union` subst'right'
--     -- NOTE: in the tutorial he composes the other way around right' union left

-- Compose substitutions
compose'subst :: Subst -> Subst -> Subst
(Sub sub'l) `compose'subst` (Sub sub'r)
  = Sub $ Map.map (apply (Sub sub'l)) sub'r `Map.union` sub'l


occurs :: String -> Type -> Bool
occurs name (TyVar varname)
  = name == varname
occurs name (TyCon conname)
  = False
occurs name (TyTuple ts)
  = any (occurs name) ts
occurs name (TyList t)
  = occurs name t
occurs name (TyArr left right)
  = occurs name left || occurs name right


-- myslim ze tohle musim zmenit na :: ... -> Solve Unifier
-- return Unifier taky funguje
bind :: String -> Type -> Solve Subst
bind varname type'
  | type' == TyVar varname  = return empty'subst
  | occurs varname type'    = throwError $ InfiniteType varname type'
  | otherwise               = return $ Sub $ Map.singleton varname type'


-- unify :: Type -> Type -> Infer Subst
-- unify (TyVar varname) type'
--   = bind varname type'
-- 
-- unify type' (TyVar varname)
--   = bind varname type'
-- 
-- unify (TyCon name'l) (TyCon name'r)
--   | name'l == name'r = return empty'subst
--   | otherwise = throwError $ UnifMismatch name'l name'r
-- 
-- unify (TyTuple ts'left) (TyTuple ts'right)
--   = if length ts'left /= length ts'right
--     then throwError $ UnifShapeMismatch (TyTuple ts'left) (TyTuple ts'right)
--     else foldM
--       (\ subst' (t'left, t'right) -> do
--         subst'new <- unify (apply subst' t'left) (apply subst' t'right)
--         return (subst' `compose'subst` subst'new))
--       empty'subst
--       (zip ts'left ts'right)
-- 
-- unify (TyList t'left) (TyList t'right)
--   = unify t'left t'right
-- 
-- unify (TyArr left'a right'a) (TyArr left'b right'b) = do
--   subst'left <- unify left'a left'b
--   subst'right <- unify (apply subst'left right'a) (apply subst'left right'b)
--   return (subst'right `compose'subst` subst'left)
-- 
-- unify t'left t'right =
--   throwError $ UnifShapeMismatch t'left t'right


generalize :: TypeEnv -> Type -> Scheme
generalize env type'
  = ForAll type'args type'
    where
      fvt = ftv type'
      fve = ftv env
      type'args = Set.toList $ fvt `Set.difference` fve


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


fresh :: Infer Type
fresh = do
  InferState { count = counter } <- get
  put $ InferState { count = counter + 1 }
  return $ TyVar (letters !! counter)


real'fresh :: [String] -> a -> Infer Type
real'fresh vars var = do
  InferState { count = counter } <- get
  put $ InferState { count = counter + 1 }
  let name = letters !! counter
  if name `elem` vars
    then real'fresh vars var
    else return $ TyVar name


instantiate :: Scheme -> Infer Type
instantiate (ForAll args type') = do
  args' <- mapM (real'fresh args) args
  let subst = Sub $ Map.fromList $ zip args args'
  return $ apply subst type'
