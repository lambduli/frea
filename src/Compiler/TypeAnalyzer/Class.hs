module Compiler.TypeAnalyzer.Class where


import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Trans.Except (catchE)


import Compiler.Syntax.Type

import Compiler.TypeAnalyzer.Error

import Compiler.TypeAnalyzer.Types
import Compiler.TypeAnalyzer.Solver
import Compiler.TypeAnalyzer.Substituable

type Supers = [Name]


type Class = (Supers, [Instance])


{- TODO: NOTE: I will most likely need to also store definitions of the methods and constants -}
type Instance = Qualified Predicate


data ClassEnv = ClassEnv  { classes :: Map.Map Name Class
                          , defaults :: [Type] }


{- TODO: NOTE: it's partial, I don't quite like that. Fix that later.-}
super :: ClassEnv -> Name -> [Name]
super cl'env name
  = case classes cl'env Map.!? name of
      Just (supers, instances) -> supers
      _ -> error "super is still a partial function"


instances :: ClassEnv -> Name -> [Instance]
instances cl'env name
  = case classes cl'env Map.!? name of
      Just (supers, instances) -> instances
      _ -> error "instances is still a partial function"



modify :: ClassEnv -> Name -> Class -> ClassEnv
modify cl'env@ClassEnv{ classes = classes } name class'
  = cl'env{ classes = Map.insert name class' classes }


initial'env :: ClassEnv
initial'env = ClassEnv { classes = Map.empty, defaults = [t'Int, t'Double] }

type EnvTransformer = ClassEnv -> Solve ClassEnv


infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) cl'env = do
  cl'env' <- f cl'env
  g cl'env'



-- TODO: REWRITE THE RESULT TYPE
-- following three functions will return something like Solve (...)
add'class :: Name -> [Name] -> EnvTransformer
add'class name names cl'env
  | isJust (classes cl'env Map.!? name) = throwError $ Unexpected "class already exists"
  | any (isNothing . (\ name -> classes cl'env Map.!? name)) names = throwError $ Unexpected "superclass not defined"
  | otherwise = return (modify cl'env name (names, []))


add'inst :: [Predicate] -> Predicate -> EnvTransformer
add'inst preds pred@(IsIn name _) cl'env
  | isNothing $ classes cl'env Map.!? name = throwError $ Unexpected "no class for instance"
  | otherwise = do
      let overlapping = anyM (overlap pred) qs
      ifM overlapping (throwError $ Unexpected "overlapping instance") (return $ modify cl'env name c)
        where
          its = instances cl'env name
          qs = [q | (_ :=> q) <- its]
          c = (super cl'env name, (preds :=> pred) : its)
-- TODO: rename most of the stuff in this function so it's aparent what is going on here


{- TODO:  fix PLS!
          What is wrong with it:  `overlap` returns Bool, but actually it relies on the Error to signalize "overlapping".
                                  In that case, it would make more sense to just return (). But then I need to use something different from `ifM`.
          What would be better: 
 -}
overlap :: Predicate -> Predicate -> Solve Bool
overlap p q = do
  p `unify` q :: Solve (Subst TVar Type)
  return False
-- takze co se tady deje
-- moje unify nevraci Maybe (Subst ...)
-- ja to mam schovany v monad transformeru abych mohl reportovat ruzny konkretni duvody, proc neslo unifikovat
-- coz je ofc lepsi, nez jenom Maybe a rict - Nothing
-- takze logicky, i tahle funkce, bude muset pracovat s mym monad transformerem
-- a tim padem i vsechny ostatni - prirozene
-- ono ostatne to dava smysl - stejne jsou to funkce, ktery budu volat kdyz uz budu uvnitr monad transf




by'super :: ClassEnv -> Predicate -> [Predicate]
by'super cl'env pred@(IsIn name type')
  = pred : concat [by'super cl'env (IsIn name' type') | name' <- super cl'env name]


by'inst :: ClassEnv -> Predicate -> Solve [Predicate]
by'inst cl'env pred@(IsIn name type') =
  let insts = instances cl'env name
  in first'defined insts
  
  -- first'defined [try'inst it | it <- instances cl'env name]
  where
      try'inst :: Instance -> Solve [Predicate]
      try'inst (preds :=> head) = do
        u <- match head pred :: Solve (Subst TVar Type)
        return (map (apply u) preds)

      first'defined :: [Instance] -> Solve [Predicate]
      first'defined [] = throwError $ Unexpected "Error: I think I didn't find any instances of the thing."
      first'defined (inst : insts) = do
        v <- try'inst inst
        catchE (try'inst inst) (const $ first'defined insts)


entail :: ClassEnv -> [Predicate] -> Predicate -> Solve Bool
entail cl'env preds pred = do
  let is'by'super = any (pred `elem`) (map (by'super cl'env) preds)
  either'is'by'inst'or'not <- tryE (by'inst cl'env pred)
  let is'by'inst = case either'is'by'inst'or'not of
                    Left _ -> False
                    Right _ -> True
  return $ is'by'super || is'by'inst



{- NOTE: For some reason this function is not exported from the module it should be exported.
        So until I figure out what's up, I will just copy it here by hand. -}
tryE :: Monad m => ExceptT e m a -> ExceptT e m (Either e a)
tryE m = catchE (liftM Right m) (return . Left)


in'hnf :: Predicate -> Bool
in'hnf (IsIn class'name type') = hnf type'
  where
    hnf (TyVar var) = True
    hnf (TyCon con) = False
    hnf (TyApp type'l _) = hnf type'l
    -- leaving out TyTuple -> I will refactor it out eventually anyway


to'hnfs :: ClassEnv -> [Predicate] -> Solve [Predicate]
to'hnfs cl'env preds = do
  predicates <- mapM (to'hnf cl'env) preds
  return $ concat predicates


to'hnf :: ClassEnv -> Predicate -> Solve [Predicate]
to'hnf cl'env pred
  | in'hnf pred = return [pred]
  | otherwise = do
    either'err'or'preds <- tryE $ by'inst cl'env pred
    case either'err'or'preds of
      Left _ -> throwError $ Unexpected "Failed in context reduction. I think some predicate is unsatisfiable."
      Right preds -> to'hnfs cl'env preds


simplify :: ClassEnv -> [Predicate] -> Solve [Predicate]
simplify cl'env = loop []
  where
    loop rs [] = return rs
    loop rs (pred : preds) = do
      entailed <- entail cl'env (rs ++ preds) pred
      if entailed
      then loop rs preds
      else loop (pred : rs) preds


reduce :: ClassEnv -> [Predicate] -> Solve [Predicate]
reduce cl'env preds = do
  qs <- to'hnfs cl'env preds
  simplify cl'env qs