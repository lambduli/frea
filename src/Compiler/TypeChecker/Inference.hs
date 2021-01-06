module Compiler.TypeChecker.Inference where

import qualified Data.Map.Strict as Map
import Data.List (elem, foldl)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except

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


type Infer a = ExceptT TypeError (State Int) a

type TypeEnv = Map.Map String Scheme

-- type SchemeEnv = Map.Map String Scheme

-- type substitution -- ordered mapping between name and type
type Subst = [(String, Type)]

data TypeError
  = InfiniteType String Type
  | UnifMismatch String String
  | UnboundVariable String
  | UnboundConstructor String
  | UnifShapeMismatch Type Type
  deriving (Show)


class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set String
-- TODO: implement instances for Type, Scheme, Substitution


extend :: TypeEnv -> (String, Scheme) -> TypeEnv
extend env (ty'var, scheme) = Map.insert ty'var scheme env



subst'var'type :: String -> Type -> Type -> Type
subst'var'type name replacement var@(TyVar varname)
  | name == varname = replacement
  | otherwise = var

-- subst'var'type name replacement tycon@(BuiltInTyCon _)
--   = tycon

-- subst'var'type name replacement (AppTy left right)
--   = AppTy (subst'var'type name replacement left) (subst'var'type name replacement right)

subst'var'type name replacement (TyCon conname)
  = (TyCon conname)

subst'var'type name replacement (TyArr left right)
  = TyArr (subst'var'type name replacement left) (subst'var'type name replacement right)

subst'var'type name replacement (TyTuple types)
  = TyTuple $ map (\ t' -> subst'var'type name replacement t') types

subst'var'type name replacement (TyList type') = case type' of
  TyVar var | name == var -> TyList replacement
  TyVar _ -> TyList type'
  _ -> TyList $ subst'var'type name replacement type'

{-
subst'var'type'scheme name replacement type'scheme
replaces all occurences of the type variable with given name in the type scheme
but only if the variable given is not one of the bound ones by the type scheme
-}
subst'var'type'scheme :: String -> Type -> Scheme -> Scheme
subst'var'type'scheme name replacement (ForAll varnames type')
  | elem name varnames = ForAll varnames type'
  | otherwise = ForAll varnames $ subst'var'type name replacement type'



runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m
 = case evalState (runExceptT m) 0 of
    Left err  -> Left err
    Right res -> Right $ closeOver res

-- helpInfer :: Infer (Subst, Type) -> Either TypeError (Subst, Type, Scheme)
-- helpInfer m
--   = case evalState (runExceptT m) 0 of
--     Left err -> Left err
--     Right (s, t) -> Right (s, t, closeOver (s, t))


closeOver :: (Subst, Type) -> Scheme
closeOver (subst, type')
  = normalize sc
  where sc = generalize Map.empty (apply'subst'type subst type')  


normalize :: Scheme -> Scheme
normalize (ForAll type'args body) = ForAll (fmap snd ord) (normtype body)
  where
    ord = zip (Set.toList . free'type'vars'type $ body) letters

    normtype (TyArr a b) = TyArr (normtype a) (normtype b)
    normtype (TyCon a) = TyCon a
    normtype (TyTuple ts) = TyTuple $ map normtype ts
    normtype (TyList t) = TyList $ normtype t
    normtype (TyVar a) =
      case lookup a ord of
        Just x -> TyVar x
        Nothing -> error $ "Type variable " ++ show a ++ " not in the signature." 



empty'subst :: Subst
empty'subst = []



-- not really empty
empty'env :: TypeEnv
empty'env = Map.fromList
  [ ("#fst",  ForAll ["a", "b"] (TyArr (TyTuple [TyVar "a", TyVar "b"]) (TyVar "a")))
  , ("#snd",  ForAll ["a", "b"] (TyArr (TyTuple [TyVar "a", TyVar "b"]) (TyVar "b")))
  , ("#=",    ForAll ["a"]      ((TyTuple [TyVar "a", TyVar "a"]) `TyArr` (TyCon "Bool")))
  , ("#<",    ForAll []         ((TyTuple [TyCon "Int", TyCon "Int"]) `TyArr` (TyCon "Bool")))
  , ("#>",    ForAll []         ((TyTuple [TyCon "Int", TyCon "Int"]) `TyArr` (TyCon "Bool")))
  , ("#+",    ForAll []         ((TyTuple [TyCon "Int", TyCon "Int"]) `TyArr` (TyCon "Int")))
  , ("#*",    ForAll []         ((TyTuple [TyCon "Int", TyCon "Int"]) `TyArr` (TyCon "Int")))
  , ("#-",    ForAll []         ((TyTuple [TyCon "Int", TyCon "Int"]) `TyArr` (TyCon "Int")))
  , ("#/",    ForAll []         ((TyTuple [TyCon "Int", TyCon "Int"]) `TyArr` (TyCon "Int")))
  -- concat two strings
  , ("#.",    ForAll []         ((TyTuple [TyCon "String", TyCon "String"]) `TyArr` (TyCon "String")))
  , ("#++",   ForAll ["a"]      ((TyTuple [TyList (TyVar "a"), TyList (TyVar "a")]) `TyArr` (TyList (TyVar "a"))))
  , ("#!!",   ForAll ["a"]      ((TyTuple [TyCon "Int", TyList (TyVar "a")]) `TyArr` (TyVar "a")))
  -- prepend element to a list
  , ("#:",    ForAll ["a"]      ((TyTuple [TyVar "a", TyList (TyVar "a")]) `TyArr` (TyList (TyVar "a"))))
  -- prepend a char to a string
  , ("#;",    ForAll []         ((TyTuple [TyCon "Char", TyCon "String"]) `TyArr` (TyCon "String")))
  -- head of the list
  , ("#head", ForAll ["a"]      ((TyList (TyVar "a")) `TyArr` (TyVar "a")))
  , ("#tail", ForAll ["a"]      ((TyList (TyVar "a")) `TyArr` (TyList (TyVar "a"))))
  , ("#nil?", ForAll ["a"]      ((TyList (TyVar "a")) `TyArr` (TyCon "Bool")))
  ]


-- apply'subst'loctype :: Subst -> LocType -> LocType
-- apply'subst'loctype subst loctype
--   = foldl (\ loctype (name, replacement) -> subst'var'type name replacement loctype) loctype subst



apply'subst'type :: Subst -> Type -> Type
apply'subst'type subst type'
  = foldl (\ type' (name, replacement) -> subst'var'type name replacement type') type' subst



apply'subst'scheme :: Subst -> Scheme -> Scheme
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
apply'subst'env :: Subst -> TypeEnv -> TypeEnv
apply'subst'env subst type'env
  = Map.map
      (\ scheme -> apply'subst'scheme subst scheme)
      type'env



-- apply'subst'env'scheme :: Subst -> SchemeEnv -> SchemeEnv
-- apply'subst'env'scheme subst scheme'env
--   = Map.map
--       (\ scheme -> apply'subst'scheme subst scheme)
--       scheme'env



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
--     TyVar (At _ name) -> singleton name
--     BuiltInTyCon _ -> empty
--     AppTy left right -> union (free'loc'type'vars'type left) (free'loc'type'vars'type right)
--     TyArr left righty -> union (free'loc'type'vars'type left) (free'loc'type'vars'type right)



free'type'vars'type :: Type -> Set.Set String
free'type'vars'type type'
  = case type' of
    TyVar name -> Set.singleton name
    TyCon name -> Set.empty
    TyTuple ts -> foldl (\ set' t' -> Set.union set' (free'type'vars'type t')) Set.empty ts
    TyList t -> free'type'vars'type t
    -- BuiltInTyCon _ -> Set.empty
    -- AppTy left right -> Set.union (free'type'vars'type left) (free'type'vars'type right)
    TyArr left right -> Set.union (free'type'vars'type left) (free'type'vars'type right)



free'type'vars'scheme :: Scheme -> Set.Set String
free'type'vars'scheme (ForAll vars type')
  = Set.difference (free'type'vars'type type') (Set.fromList vars)



free'type'vars'type'env :: TypeEnv -> Set.Set String
free'type'vars'type'env type'env
  = Map.foldr
      (\ scheme free'vars -> Set.union free'vars (free'type'vars'scheme scheme))
      Set.empty
      type'env



occurs :: String -> Type -> Bool
occurs name (TyVar varname)
  = name == varname
occurs name (TyCon conname)
  = False
occurs name (TyTuple ts)
  = any (occurs $ name) ts
occurs name (TyList t)
  = occurs name t
-- occurs name (BuiltInTyCon _)
  -- = False
-- occurs name (AppTy left right)
--   = occurs name left || occurs name right
occurs name (TyArr left right)
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
unify :: Type -> Type -> Infer Subst
unify (TyVar varname) type'
  = if occurs varname type'
    then throwError $ InfiniteType varname type'
    else return $ [(varname, type')]

unify type' (TyVar varname)
  = if occurs varname type'
    then throwError $ InfiniteType varname type'
    else return $ [(varname, type')]

unify (TyCon name'l) (TyCon name'r)
  | name'l == name'r = return empty'subst
  | otherwise = throwError $ UnifMismatch name'l name'r

-- unify (TyTuple [left'a, left'b]) (TyTuple [right'a, right'b]) = do
--   subst'a <- unify left'a right'a
--   subst'b <- unify (apply'subst'type subst'a left'b) (apply'subst'type subst'a right'b)
--   return (subst'a `compose'subst` subst'b)

unify (TyTuple ts'left) (TyTuple ts'right)
  = if length ts'left /= length ts'right
    then throwError $ UnifShapeMismatch (TyTuple ts'left) (TyTuple ts'right) 
    else foldM
      (\ subst' (t'left, t'right) -> do
        subst'new <- unify (apply'subst'type subst' t'left) (apply'subst'type subst' t'right)
        return (subst' `compose'subst` subst'new))
      empty'subst
      (zip ts'left ts'right)

unify (TyList t'left) (TyList t'right)
  = unify t'left t'right

-- unify (BuiltInTyCon UnitTyCon) (BuiltInTyCon UnitTyCon)
--   = return empty'subst

unify (TyArr left'a right'a) (TyArr left'b right'b) = do
  subst'left <- unify left'a left'b
  subst'right <- unify (apply'subst'type subst'left right'a) (apply'subst'type subst'left right'b)
  return (subst'right `compose'subst` subst'left)

  -- case unify left'a left'b of
  --   Right left'subst ->
  --     let
  --       right'a'subed = apply'subst'type left'subst right'a
  --       right'b'subed = apply'subst'type left'subst right'b
  --       in case unify right'a'subed right'b'subed of
  --         Right right'subst -> Right $ 'subst'subst left'subst right'subst
  --         reason -> reason
  --   reason -> reason

unify t'left t'right = do
  throwError $ UnifShapeMismatch t'left t'right

{-
Napadlo me:
Slo by zkombinovat parsovani a type checkovani?
-}


generalize :: TypeEnv -> Type -> Scheme
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
  return $ TyVar (letters !! counter)



instantiate :: Scheme -> Infer Type
instantiate (ForAll args type') = do
  args' <- mapM (const fresh) args
  let subst = zip args args'
  return $ apply'subst'type subst type'



infer :: TypeEnv -> Expression -> Infer (Subst, Type)
-- infer = undefined
infer env expr = case expr of
  Var x ->
    case Map.lookup x env of
      Nothing -> throwError $ UnboundVariable x
      Just scheme -> do
        type' <- instantiate scheme
        return (empty'subst, type')

  Op x ->
    case Map.lookup x env of
      Nothing -> throwError $ UnboundVariable x
      Just scheme -> do
        type' <- instantiate scheme
        return (empty'subst, type')
      -- "#=" -> return (empty'subst, ((TyTuple [TyVar "a", TyVar "a"]) `TyArr` (TyCon "Bool")))
      -- "#<" -> return (empty'subst, ((TyTuple [TyCon "Int", TyCon "Int"]) `TyArr` (TyCon "Bool")))
      -- "#>" -> return (empty'subst, ((TyTuple [TyCon "Int", TyCon "Int"]) `TyArr` (TyCon "Bool")))
      -- "#+" -> return (empty'subst, ((TyTuple [TyCon "Int", TyCon "Int"]) `TyArr` (TyCon "Int")))
      -- "#*" -> return (empty'subst, ((TyTuple [TyCon "Int", TyCon "Int"]) `TyArr` (TyCon "Int")))
      -- "#-" -> return (empty'subst, ((TyTuple [TyCon "Int", TyCon "Int"]) `TyArr` (TyCon "Int")))
      -- "#/" -> return (empty'subst, ((TyTuple [TyCon "Int", TyCon "Int"]) `TyArr` (TyCon "Int")))
      -- concat two strings
      -- "#++" -> return (empty'subst, ((TyTuple [TyCon "String", TyCon "String"]) `TyArr` (TyCon "String")))
      -- prepend a char to a string
      -- "#:" -> return (empty'subst, ((TyTuple [TyCon "Char", TyCon "String"]) `TyArr` (TyCon "String")))
      -- append a char to a string
      -- "#;" -> return (empty'subst, ((TyTuple [TyCon "Char", TyCon "String"]) `TyArr` (TyCon "String")))
      -- "#fst" -> return (empty'subst, ((TyTuple [TyVar "x", TyVar "y"]) `TyArr` (TyVar "x")))
      -- "#snd" -> return (empty'subst, ((TyTuple [TyVar "a", TyVar "b"]) `TyArr` (TyVar "b")))

  Lam x body -> do
    type'var <- fresh
    let env' = env `extend` (x, ForAll [] type'var)
    (subst', type') <- infer env' body
    return (subst', apply'subst'type subst' (type'var `TyArr` type'))

  App left right -> do
    type'var <- fresh
    (subst'left, type'left) <- infer env left
    (subst'right, type'right) <- infer (apply'subst'env subst'left env) right
    subst' <- unify (apply'subst'type subst'right type'left) (type'right `TyArr` type'var)
    return (subst' `compose'subst` subst'right `compose'subst` subst'left, apply'subst'type subst' type'var)

  Con x ->
    case Map.lookup x env of
      Nothing -> throwError $ UnboundConstructor x
      Just scheme -> do
        type' <- instantiate scheme
        return (empty'subst, type')

  If cond' then' else' -> do
    -- type'var <- fresh
    (subst'cond, type'cond) <- infer env cond'
    let env' = apply'subst'env subst'cond env
    (subst'then', type'then') <- infer env' then'
    let env'' = apply'subst'env subst'then' env'
    (subst'else', type'else') <- infer env'' else'
    -- let env''' = apply'subst'env subst'else' env''

    let subst' = subst'cond `compose'subst` subst'then' `compose'subst` subst'else'

    let cond'type' = apply'subst'type subst' type'cond
    let then'type' = apply'subst'type subst' type'then'
    let else'type' = apply'subst'type subst' type'else'

    unif'subst'1 <- unify cond'type' (TyCon "Bool")
    unif'subst'2 <- unify then'type' else'type'

    let final'subst = subst' `compose'subst` unif'subst'1 `compose'subst` unif'subst'2

    let final'type = apply'subst'type final'subst then'type' -- or else'type' both should work I think
    return (final'subst, final'type)
  
  -- TODO: finish Let, Fix later

  Let name value expression -> do
    (subst'val, type'val) <- infer env value
    let env' = apply'subst'env subst'val env
    let type'val' = generalize env' type'val
    (subst'expr, type'expr) <- infer (env' `extend` (name, type'val')) expression
    return (subst'expr `compose'subst` subst'val, type'expr)

  -- Tuple [left, right] -> do
  --   (subst'left, type'left) <- infer env left
  --   let env' = apply'subst'env subst'left env
  --   (subst'right, type'right) <- infer env' right
  --   let subst'comp = subst'left `compose'subst` subst'right

  --   let type'left'fin = apply'subst'type subst'comp type'left
  --   let type'right'fin = apply'subst'type subst'comp type'right

  --   return (subst'comp, TyTuple [type'left'fin, type'right'fin])

  Tuple exprs -> do
    (subst'fin, env'fin, types) <- foldM infer' (empty'subst, env, []) exprs
    let types'fin = map (\ t -> apply'subst'type subst'fin t) types
    return (subst'fin, TyTuple types'fin)
      where
        infer' (sub, env, ts) exp' = do
          (subst', type') <- infer env exp'
          let env' = apply'subst'env subst' env
          return (sub `compose'subst` subst', env', ts ++ [type'])
    -- hopefuly it's correct

  List exprs -> do
    (subst', env'fin, types) <- foldM infer' (empty'subst, env, []) exprs
    let types' = map (\ t -> apply'subst'type subst' t) types
    type'var <- fresh
    (subst'fin, type'fin) <- foldM unify' (empty'subst, type'var) types'
    return (subst'fin, TyList type'fin)
      where
        infer' (sub, env, ts) exp' = do
          (subst', type') <- infer env exp'
          let env' = apply'subst'env subst' env
          return (sub `compose'subst` subst', env', ts ++ [type'])
        unify' (sub, t) t' = do
          sub' <- unify (apply'subst'type sub t) (apply'subst'type sub t') -- the first apply shouldn't be ncessary, but won't hurt
          return (sub `compose'subst` sub', apply'subst'type sub' t)

  Fix expr -> do
    type'var <- fresh
    let t' = (type'var `TyArr` type'var) `TyArr` type'var
    (sub, t) <- infer env expr
    type'var' <- fresh
    sub' <- unify (t `TyArr` type'var') t'
    return (sub' `compose'subst` sub, apply'subst'type sub' type'var')
    -- TODO: continue here
    

  Lit (LitInt i) -> return (empty'subst, (TyCon "Int"))
  Lit (LitDouble d) -> return (empty'subst, (TyCon "Double"))
  Lit (LitChar ch) -> return (empty'subst, (TyCon "Char"))
  Lit (LitString s) -> return (empty'subst, (TyCon "String"))
  Lit (LitBool b) -> return (empty'subst, (TyCon "Bool"))


inferExpression :: TypeEnv -> Expression -> Either TypeError Scheme
inferExpression env = runInfer . infer env

inferTop :: TypeEnv -> [(String, Expression)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpression env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

typeof :: TypeEnv -> String -> Maybe Scheme
typeof env name = Map.lookup name env
