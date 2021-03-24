module Compiler.TypeChecker.Inference where

import qualified Data.Map.Strict as Map
import Data.List (elem, foldl)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Bifunctor (second)
import Data.Functor.Identity

import Compiler.TypeChecker.Type 
import Compiler.Syntax.Declaration
import Compiler.TypeChecker.Utils


import Compiler.Syntax
  ( Bind(..)
  , Declaration(..), ConstrDecl(..)
  , Expression(..)
  , Lit(..)
  , MatchGroup(..), Match(..)
  , Pattern(..)
  , Sig(..)
  , Type(..), Scheme(..))

import Compiler.Syntax.Expression
import Compiler.TypeChecker.TypeError


-- Extend type environment
inEnv :: (String, Scheme) -> Infer a -> Infer a
inEnv (var, scheme) m = do
  let scope e = remove e var `extend` (var, scheme)
  local scope m


-- Lookup type in the environment
lookupEnv :: String -> Infer Type
lookupEnv x = do
  (Env env) <- ask
  case Map.lookup x env of
      Nothing   ->  throwError $ UnboundVariable x
      Just s    ->  instantiate s








-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: TypeEnv -> Expression -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env expr = case runInfer env (infer expr) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ apply subst ty







infer :: Expression -> Infer (Type, [Constraint])
infer expr = case expr of  
  Lit (LitInt i) -> return (t'Int, [])
  Lit (LitDouble d) -> return (t'Double, [])
  Lit (LitChar ch) -> return (t'Char, [])
  Lit (LitString s) -> return (TyList t'Char, [])
  Lit (LitBool b) -> return (t'Bool, [])
  Lit LitUnit -> return (t'Unit, [])

  (Var x) -> do
    type' <- lookupEnv x
    return (type', [])

  Op x -> do
    type' <- lookupEnv x
    return (type', [])

  Lam x body -> do
    t'var <- fresh
    (t, constrs) <- inEnv (x, ForAll [] t'var) (infer body)
    return (t'var `TyArr` t, constrs)

  App left right -> do
    (t'l, cs'l) <- infer left
    (t'r, cs'r) <- infer right
    t'var <- fresh
    return (t'var, cs'l ++ cs'r ++ [(t'l, t'r `TyArr` t'var)])

  If cond tr fl -> do
    (t1, c1) <- infer cond
    (t2, c2) <- infer tr
    (t3, c3) <- infer fl
    return (t2, c1 ++ c2 ++ c3 ++ [(t1, t'Bool), (t2, t3)])
  
  Let x ex'val ex'body -> do
    env <- ask
    (t'val, cs'val) <- infer ex'val
    case runSolve cs'val of
        Left err -> throwError err
        Right sub -> do
            let sc = generalize (apply sub env) (apply sub t'val)
            (t'body, cs'body) <- inEnv (x, sc) $ local (apply sub) (infer ex'body)
            return (t'body, cs'val ++ cs'body)

  Fix expr -> do
    (type', cs) <- infer expr
    t'var <- fresh
    return (t'var, cs ++ [(t'var `TyArr` t'var, type')])

  Tuple exprs -> do
    -- co potrebuju je projit vsechny expressions
    -- pro kazdej z nich zavolat infer a tim ziskam vzdycky type a list constraintu
    -- ten type je to co by byl typ toho vyrazu a list constraintu je list
    -- podminek, ktery musi byt splneny, aby ten typ byl skutecne validni
    -- takze az budu foldovat list expressions tak pro kazdy kolo budu muset
    -- vyrobit ten type a list constraintu
    -- a to by melo uplne stacit
    -- puvodne jsem nejdriv foldoval, tim jsem vytvarel novy substitution
    -- environment jenom posouvam a vzdycky na nej aplikuju novou substituci
    -- a posledni co vytvarim je list typu
    -- takze kdyz dropnu environment a substituce - protoze ty obstara az reseni constraintu
    -- tak jediny co me ted trapi je vytvorit list typu
    -- a vytvorit list constraintu
    -- constrainty budou proste jeden silene dlouhej list constraintu
    -- typy vzniknou efektivne jenom mapovanim - ale kdyz uz pouzivam foldl tak to udelam najednou
    (types, cs) <- foldM infer' ([], []) exprs
    return (TyTuple $ reverse types, cs)
      where
        infer' (types, constrs) expr = do
          (t, cs) <- infer expr
          return (t : types, cs ++ constrs)

  List exprs -> do
    type'var <- fresh
    let infer' costrs expr = do
          (t, cs) <- infer expr
          return $ (type'var, t) : cs
    constrs <- foldM infer' [] exprs
    return (TyList type'var, constrs)


-- infer (Env env) expr = case expr of
  -- Var x ->
  --   case Map.lookup x env of
  --     Nothing -> throwError $ UnboundVariable x
  --     Just scheme -> do
  --       type' <- instantiate scheme
  --       return (empty'subst, type')

  -- Op x ->
  --   case Map.lookup x env of
  --     Nothing -> throwError $ UnboundVariable x
  --     Just scheme -> do
  --       type' <- instantiate scheme
  --       return (empty'subst, type')

  -- Lam x body -> do
  --   type'var <- fresh
  --   let env' = Env env `extend` (x, ForAll [] type'var)
  --   (subst', type') <- infer env' body
  --   return (subst', apply subst' (type'var `TyArr` type'))

  -- App left right -> do
  --   type'var <- fresh
  --   (subst'left, type'left) <- infer (Env env) left
  --   (subst'right, type'right) <- infer (apply subst'left (Env env)) right
  --   subst' <- unify (apply subst'right type'left) (type'right `TyArr` type'var)
  --   return (subst' `compose'subst` subst'right `compose'subst` subst'left, apply subst' type'var)

  -- If cond' then' else' -> do
  --   (subst'cond, type'cond) <- infer (Env env) cond'
  --   let env' = apply subst'cond (Env env)
  --   (subst'then', type'then') <- infer env' then'
  --   let env'' = apply subst'then' env'
  --   (subst'else', type'else') <- infer env'' else'

  --   let subst' = subst'cond `compose'subst` subst'then' `compose'subst` subst'else'

  --   let cond'type' = apply subst' type'cond
  --   let then'type' = apply subst' type'then'
  --   let else'type' = apply subst' type'else'

  --   unif'subst'1 <- unify cond'type' t'Bool
  --   unif'subst'2 <- unify then'type' else'type'

  --   let final'subst = subst' `compose'subst` unif'subst'1 `compose'subst` unif'subst'2

  --   let final'type = apply final'subst then'type' -- or else'type' both should work I think
  --   return (final'subst, final'type)

  -- Let name value expression -> do
  --   (subst'val, type'val) <- infer (Env env) value
  --   let env' = apply subst'val (Env env)
  --   let type'val' = generalize env' type'val
  --   (subst'expr, type'expr) <- infer (env' `extend` (name, type'val')) expression
  --   return (subst'expr `compose'subst` subst'val, type'expr)

  -- Tuple exprs -> do
  --   (subst'fin, env'fin, types) <- foldM infer' (empty'subst, Env env, []) exprs
  --   let types'fin = map (apply subst'fin) types
  --   return (subst'fin, TyTuple types'fin)
  --     where
  --       infer' (sub, env, ts) exp' = do
  --         (subst', type') <- infer env exp'
  --         let env' = apply subst' env
  --         return (sub `compose'subst` subst', env', ts ++ [type'])

  -- List exprs -> do
  --   (subst', env'fin, types) <- foldM infer' (empty'subst, Env env, []) exprs
  --   let types' = map (apply subst') types
  --   type'var <- fresh
  --   (subst'fin, type'fin) <- foldM unify' (empty'subst, type'var) types'
  --   return (subst'fin, TyList type'fin)
  --     where
  --       infer' (sub, env, ts) exp' = do
  --         (subst', type') <- infer env exp'
  --         let env' = apply subst' env
  --         return (sub `compose'subst` subst', env', ts ++ [type'])
  --       unify' (sub, t) t' = do
  --         sub' <- unify (apply sub t) (apply sub t') -- the first apply shouldn't be ncessary, but won't hurt
  --         return (sub `compose'subst` sub', apply sub' t)

  -- Fix expr -> do
  --   type'var <- fresh
  --   let t' = (type'var `TyArr` type'var) `TyArr` type'var
  --   (sub, t) <- infer (Env env) expr
  --   type'var' <- fresh
  --   sub' <- unify (t `TyArr` type'var') t'
  --   return (sub' `compose'subst` sub, apply sub' type'var')

  -- Lit (LitInt i) -> return (empty'subst, t'Int)
  -- Lit (LitDouble d) -> return (empty'subst, t'Double)
  -- Lit (LitChar ch) -> return (empty'subst, t'Char)
  -- Lit (LitString s) -> return (empty'subst, TyList t'Char)
  -- Lit (LitBool b) -> return (empty'subst, t'Bool)
  -- Lit LitUnit -> return (empty'subst, t'Unit)
