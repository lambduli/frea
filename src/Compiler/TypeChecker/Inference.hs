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
import Compiler.Syntax.Literal
import Compiler.Syntax.Type
import Compiler.Syntax.Expression
import Compiler.TypeChecker.TypeError
import Compiler.TypeChecker.Inference.Infer
import Compiler.TypeChecker.Inference.Solver
import Compiler.TypeChecker.Inference.Substituable
import Compiler.TypeChecker.Inference.TypeEnv
import Compiler.TypeChecker.Inference.Constraint
import Compiler.TypeChecker.Inference.InferState
import Compiler.TypeChecker.Inference.InferUtils


check :: Type -> Expression -> Infer ((), [Constraint])
check (TyVar n) e@(Lit _) = throwError $ TEMismatch (TyVar n) (show e)
check t'Int (Lit (LitInt i)) = return ((), [])
check t'Double (Lit (LitDouble i)) = return ((), [])
check t'Char (Lit (LitChar i)) = return ((), [])

check t (Var x) = do
  -- for now just assume t is valid type of kind *
  type' <- lookup'env x
  return ((), [(t, type')])

check t (Op x) = do
  -- assume t :: *
  type' <- lookup'env x
  return ((), [(t, type')])

check (from `TyArr` to) (Lam x body) = do
  -- assume from :: * and to :: *
  (t, constrs) <- put'in'env (x, ForAll [] from) (check to body)
  return ((), constrs)

check t (App left right) = do
  -- assume t :: *
  (t'l, cs'l) <- infer left
  (t'r, cs'r) <- infer right
  t'var <- fresh
  return ((), (t, t'var) : (t'l, t'r `TyArr` t'var) : cs'l ++ cs'r)

check t (If cond tr fl) = do
  -- assume t :: *
  (t1, c1) <- infer cond
  ((), c2) <- check t tr
  ((), c3) <- check t fl
  return ((), (t1, t'Bool) : c1 ++ c2 ++ c3)

check t (Let x ex'val ex'body) = do
  -- assume t :: *
  env <- ask
  (t'val, cs'val) <- infer ex'val
  case runSolve cs'val of
      Left err -> throwError err
      Right sub -> do
          let sc = generalize (apply sub env) (apply sub t'val)
          ((), cs'body) <- put'in'env (x, sc) $ local (apply sub) (check t ex'body)
          return ((), cs'val ++ cs'body)


check (TyTuple types') (Tuple exprs) = do
  -- assume each type :: * where type isfrom types'
  cs <- foldM check' [] (zip types' exprs)
  return ((), cs)
    where
      check' constrs (ty, expr) = do
        ((), cs) <- check ty expr
        return (cs ++ constrs)


check _ (Fix _) = throwError $ Unexpected "I am not type checking Fix expressions right now."


infer :: Expression -> Infer (Type, [Constraint])
infer expr = case expr of  
  Lit (LitInt i) -> return (t'Int, [])
  Lit (LitDouble d) -> return (t'Double, [])
  Lit (LitChar ch) -> return (t'Char, [])

  (Var x) -> do
    type' <- lookup'env x
    return (type', [])

  Op x -> do
    type' <- lookup'env x
    return (type', [])

  Lam x body -> do
    t'var <- fresh
    (t, constrs) <- put'in'env (x, ForAll [] t'var) (infer body)
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
    return (t2, (t1, t'Bool) : (t2, t3) : c1 ++ c2 ++ c3)
  
  Let x ex'val ex'body -> do
    env <- ask
    (t'val, cs'val) <- infer ex'val
    case runSolve cs'val of
        Left err -> throwError err
        Right sub -> do
            let sc = generalize (apply sub env) (apply sub t'val)
            (t'body, cs'body) <- put'in'env (x, sc) $ local (apply sub) (infer ex'body)
            return (t'body, cs'val ++ cs'body)

  Fix expr -> do
    (type', cs) <- infer expr
    t'var <- fresh
    return (t'var, cs ++ [(t'var `TyArr` t'var, type')])

  Tuple exprs -> do
    (types, cs) <- foldM infer' ([], []) exprs
    return (TyTuple $ reverse types, cs)
      where
        infer' (types, constrs) expr = do
          (t, cs) <- infer expr
          return (t : types, cs ++ constrs)
  
  Ann type' expr -> do
    let scheme = generalize empty't'env type'
    t' <- instantiate scheme
    (_, constrs) <- check t' expr
    return (type', constrs)

    -- co ted?
    -- expr vlastne muze bejt uplne cokoliv
    -- type' taky libovolny type
    -- asi bych ted mel zavolat "check", kde vlastne bude opposite pro "infer"
    -- kazdej expr a jeho odpovidajici type se budou kontrolovat
    -- v cem je checkovani tolik odlisny od inference?
    -- v tom, ze kdyz to uspesne sedi strukturalne, tak muze do Type Contextu zanaset typovy promenny
    -- samozrejme taky to prinasi potrebu zkontrolovat, ze Type ma Kind *
    -- ale to je jednoduchy
    -- uzivatelsky typovy promenny budou slozitejsi
    -- jednak se musim postarat o to, abych spravne ten type instancioval
      -- nejdriv z nej udelam scheme pomoci prazdnyho TyEnv a pak ho instanciuju
      -- tim se vyvaruju konfliktu s uz existujicima neznamyma
    -- no a pak je checking pomerne jednoduchej
    -- kdyz sedi struktura typu a vyrazu, pridavam tu informaci do contextu a type checkuju rekurzivne

    -- otazka: co kdyz napisu tohle
    -- foo :: Int -> a
    -- foo n = True
    -- True neni v tenhle moment forall a . a zejo
    -- padne to az kdyz dojde na ten literal?
    -- je mozny, ze existuje pravidlo, ktery rika, ze rigid type variable se muze
    -- unifikovat jenom s jinou type variable?
    -- 


-- Return the internal constraints used in solving for the type of an expression
-- for debugging ?
constraintsExpr :: TypeEnv -> Expression -> Either TypeError ([Constraint], Subst, Type, Scheme)
constraintsExpr env expr = case runInfer env (infer expr) of
  Left err -> Left err
  Right (type', constraints) -> case runSolve constraints of
    Left err -> Left err
    Right subst -> Right (constraints, subst, type', scheme)
      where
        scheme = closeOver $ apply subst type'
