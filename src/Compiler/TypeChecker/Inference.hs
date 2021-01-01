module Compiler.TypeChecker.Inference where

import qualified Data.Map.Strict as Map
import Data.List (elem, foldl)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except

import Compiler.Syntax ( Bind(..)
  , Declaration(..), ConstrDecl(..)
  , Expression(..)
  , Lit(..)
  , MatchGroup(..), Match(..)
  , Pattern(..)
  , Sig(..)
  , Type(..), BuiltInTyCon(..), TypeScheme(..)
  )

type Infer a = ExceptT TypeError (State Int) a


type EnvType = Map.Map String Type

type EnvTypeScheme = Map.Map String TypeScheme


{-
subst'var'type name replacement target
replaces all the occurences of the type variable with given name with the given replacement
currently user can't construct their own forall types, so no type variable binding  -}
-- subst'var'loc'type :: String -> LocType -> LocType -> LocType
-- subst'var'loc'type name replacement var@(At _ (VarTy (At pos varname)))
--   | name == varname = At pos replacement
--   | otherwise = var

-- subst'var'loc'type name replacement tycon@(At _ (BuiltInTyCon _))
--   = tycon

-- subst'var'loc'type name replacement (At pos (AppTy left right))
--   = At pos (AppTy (subst'var'loc'type name replacement left) (subst'var'loc'type name replacement right))

-- subst'var'loc'type name replacement (At pos (FunTy left right))
--   = At pos (FunTy (subst'var'loc'type name replacement left) (subst'var'loc'type name replacement right))



subst'var'type :: String -> Type -> Type -> Type
subst'var'type name replacement var@(VarTy varname)
  | name == varname = replacement
  | otherwise = var

subst'var'type name replacement tycon@(BuiltInTyCon _)
  = tycon

subst'var'type name replacement (AppTy left right)
  = AppTy (subst'var'type name replacement left) (subst'var'type name replacement right)

subst'var'type name replacement (FunTy left right)
  = FunTy (subst'var'type name replacement left) (subst'var'type name replacement right)



{-
subst'var'type'scheme name replacement type'scheme
replaces all occurences of the type variable with given name in the type scheme
but only if the variable given is not one of the bound ones by the type scheme
-}
subst'var'type'scheme :: String -> Type -> TypeScheme -> TypeScheme
subst'var'type'scheme name replacement (ForAll varnames type')
  | elem name varnames = ForAll varnames type'
  | otherwise = ForAll varnames $ subst'var'type name replacement type'



-- type substitution -- ordered mapping between name and type
type Subst = [(String, Type)]

data TypeError
  = Occurs String Type
  | UnifMismatch String String


runInfer :: Infer (Subst, Type) -> Either TypeError TypeScheme
runInfer m
 = case evalState (runExceptT m) 0 of
    Left err  -> Left err
    Right res -> Right $ closeOver res



closeOver :: (Subst, Type) -> TypeScheme
closeOver (subst, type')
  = normalize sc
  where sc = generalize Map.empty (apply'subst'type subst type')  


normalize :: TypeScheme -> TypeScheme
normalize = undefined




empty'subst :: Subst
empty'subst = []


-- apply'subst'loctype :: Subst -> LocType -> LocType
-- apply'subst'loctype subst loctype
--   = foldl (\ loctype (name, replacement) -> subst'var'type name replacement loctype) loctype subst



apply'subst'type :: Subst -> Type -> Type
apply'subst'type subst type'
  = foldl (\ type' (name, replacement) -> subst'var'type name replacement type') type' subst



apply'subst'scheme :: Subst -> TypeScheme -> TypeScheme
apply'subst'scheme subst scheme
  = foldl
      (\ scheme (name, replacement)
        -> subst'var'type'scheme name replacement scheme)
      scheme
      subst



-- this maps the type environment to a new one substituting all the substitutions
-- into environment values
-- assuming that environment keys are names of the functions or other expressions with given types
-- thus not actually caring about the keyes/names now
apply'subst'env :: Subst -> EnvType -> EnvType
apply'subst'env subst type'env
  = Map.map
      (\ type' -> apply'subst'type subst type')
      type'env



apply'subst'env'scheme :: Subst -> EnvTypeScheme -> EnvTypeScheme
apply'subst'env'scheme subst scheme'env
  = Map.map
      (\ scheme -> apply'subst'scheme subst scheme)
      scheme'env



apply'subst'subst :: Subst -> Subst -> Subst
apply'subst'subst subst'left subst'right
  = map
      (\ (name, type') -> (name, apply'subst'type subst'left type'))
      subst'right


compose'subst :: Subst -> Subst -> Subst
compose'subst subst'left subst'right
  = subst'left ++ (apply'subst'subst subst'left subst'right)


-- free'loc'type'vars'type :: LocType -> Set String
-- free'loc'type'vars'type (At _ type')
--   = case type' of
--     VarTy (At _ name) -> singleton name
--     BuiltInTyCon _ -> empty
--     AppTy left right -> union (free'loc'type'vars'type left) (free'loc'type'vars'type right)
--     FunTy left righty -> union (free'loc'type'vars'type left) (free'loc'type'vars'type right)



free'type'vars'type :: Type -> Set.Set String
free'type'vars'type type'
  = case type' of
    VarTy name -> Set.singleton name
    ConTy name -> Set.empty
    BuiltInTyCon _ -> Set.empty
    AppTy left right -> Set.union (free'type'vars'type left) (free'type'vars'type right)
    FunTy left right -> Set.union (free'type'vars'type left) (free'type'vars'type right)



free'type'vars'scheme :: TypeScheme -> Set.Set String
free'type'vars'scheme (ForAll vars type')
  = Set.difference (free'type'vars'type type') (Set.fromList vars)



free'type'vars'type'env :: EnvType -> Set.Set String
free'type'vars'type'env type'env
  = Map.foldr
      (\ type' free'vars -> Set.union free'vars (free'type'vars'type type'))
      Set.empty
      type'env



occurs :: String -> Type -> Bool
occurs name (VarTy varname)
  = name == varname
occurs name (ConTy conname)
  = False
occurs name (BuiltInTyCon _)
  = False
occurs name (AppTy left right)
  = occurs name left || occurs name right
occurs name (FunTy left right)
  = occurs name left || occurs name right




{-
This function is weird, I think the implementation allows for mistakes.
If I come to the substitution A' = Int in one part of the arrow type and in the other part
I decide that A' = Bool nothing really stops me from doing so.
I either need to apply the substitution each time I find one -> effectively removing all occurences
of decided variable -> therefore making it impossible to unify it with anything other that the
single original type OR I need to take into account substitution so far - take it as an argument too
and always first look there before making any substitution or type error.
-}
unify :: Type -> Type -> Either TypeError Subst
unify (VarTy varname) type'
  = if occurs varname type'
    then Left $ Occurs varname type'
    else Right $ [(varname, type')]

unify type' (VarTy varname)
  = if occurs varname type'
    then Left $ Occurs varname type'
    else Right $ [(varname, type')]

unify (ConTy name'l) (ConTy name'r)
  | name'l == name'r = Right empty'subst
  | otherwise = Left $ UnifMismatch name'l name'r

unify (BuiltInTyCon UnitTyCon) (BuiltInTyCon UnitTyCon)
  = Right empty'subst

unify (FunTy left'a right'a) (FunTy left'b right'b)
  = case unify left'a left'b of
      Right left'subst ->
        let
          right'a'subed = apply'subst'type left'subst right'a
          right'b'subed = apply'subst'type left'subst right'b
          in case unify right'a'subed right'b'subed of
            Right right'subst -> Right $ compose'subst left'subst right'subst
            reason -> reason
      reason -> reason

{-
Napadlo me:
Slo by zkombinovat parsovani a type checkovani?
-}


generalize :: EnvType -> Type -> TypeScheme
generalize env type'
  = ForAll type'args type'
    where
      fvt = free'type'vars'type type'
      fve = free'type'vars'type'env env
      type'args = Set.toList $ fvt `Set.difference` fve



letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']



fresh :: Infer Type
fresh = do
  counter <- get
  put $ counter + 1
  return $ VarTy (letters !! counter)



instantiate :: TypeScheme -> Infer Type
instantiate (ForAll args type') = do
  args' <- mapM (const fresh) args
  let subst = zip args args'
  return $ apply'subst'type subst type'



infer :: EnvType -> Expression -> Infer (Subst, Type)
infer = undefined
-- infer env expr = case expr of
--   Var x -> lookupEnv env x

--   Con x -> lookupEnv env x

--   Lam 