module Compiler.TypeChecker.TypeOf where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Graph (SCC(..), stronglyConnComp)
import Data.Map.Strict ((!), (!?))
import Data.List (partition)
import Data.Bifunctor

import Control.Monad
import Control.Monad.Reader

import Compiler.Syntax.Declaration
import Compiler.Syntax.Type
import Compiler.Syntax.Kind
import Compiler.Syntax.Expression

import Compiler.TypeChecker.Error
import Compiler.TypeChecker.Analize
import Compiler.TypeChecker.Solver
import Compiler.TypeChecker.Substituable
import Compiler.TypeChecker.Constraint
import Compiler.TypeChecker.AnalizeEnv
import Compiler.TypeChecker.AnalizeUtils

import qualified Compiler.TypeChecker.Type.Evaluate as E


{-  This function does the type analysis on the whole module represented as a list of Declarations -}
{-
Potrebuju napsat funkci, ktera vezme vsechny deklarace a type'env, kind'env, memory, env a ali'env

pak musim provest vsechno z tohohle:

1 Projit deklarace `data` a vygenerovat pro ne lambda constructory, pridat typy a podobne
zkratka jde o rezii, ktera se ted dela v modulu DeclarationCheck.

2 Projit deklarace `type` synonyms a zanest je do ali'env + evaluovat vsechny typy
znormalizovat typovy termy tak, aby neobsahovali synonyma ale jejich obsah

Tady je na miste otazka - nemel bych vygenerovat constrainty i ve stavu, kde jeste synonyma jsou,
pak je teprve vyhodnotit, znova vygenerovat dalsi synonyma spojit dohromady a pak teprve resit?

3 Projit deklarace constant a funkci. Provest type inferenci a type checking.

-}

analyze'module :: [Declaration] -> TypeEnv -> Either Error TypeEnv
analyze'module binds t'env = do
  let 
      (only'aliases, rest) = partition is'alias binds
      -- get all the aliases
      only'funs = filter is'fun rest
      -- take only functions

      -- ted z tech aliasu udelam kontext a normalizuju typy v only'funs
      -- NOW, map all Bindings and Annotated -> expand the aliases
      ali'env = make'alias'env only'aliases
      expanded = map (expand'aliases ali'env) only'funs
      pairs = map to'pair expanded
  infer'top t'env pairs
    where
      is'fun :: Declaration -> Bool
      is'fun (Binding _ _) = True
      is'fun (Annotated _ _ _) = True
      is'fun _ = False

      -- DUPLICATION
      expand'aliases :: Map.Map String Type -> Declaration -> Declaration
      expand'aliases ali'env (Binding name expr) = Binding name $ expand'expr ali'env expr
      expand'aliases ali'env a@(Annotated name type' expr) =
        let Right norm'type' = run'norm ali'env (E.evaluate type')
            expanded'type = norm'type'
            expanded'expr = expand'expr ali'env expr
        in Annotated name expanded'type expanded'expr
      expand'aliases _ impossible = impossible

      -- DUPLICATION
      is'alias (TypeAlias _ _) = True
      is'alias _ = False

      -- DUPLICATION
      make'alias'env decls = Map.fromList $ map (\ (TypeAlias name type') -> (name, type')) decls

      to'pair :: Declaration -> (String, Expression)
      to'pair (Binding name expr) = (name, expr)
      to'pair (Annotated name type' expr) = (name, Ann type' expr)


{-  This function finds all type annotations in the given Expression
    and evaluates them.
-}
expand'expr :: Map.Map String Type -> Expression -> Expression
expand'expr ali'env expr =
  case expr of
    Var name -> Var name
    Op name -> Op name
    Lit lit -> Lit lit
    Lam par body -> Lam par $ expand'expr ali'env body
    App left right -> App (expand'expr ali'env left) (expand'expr ali'env right)
    Tuple exprs -> Tuple $ map (expand'expr ali'env) exprs
    If cond' then' else' -> If (expand'expr ali'env cond') (expand'expr ali'env then') (expand'expr ali'env else')
    Let name value expr -> Let name (expand'expr ali'env value) (expand'expr ali'env expr)
    Fix expr -> Fix $ expand'expr ali'env expr
    Ann type' expr ->
      let Right norm'type = run'norm ali'env (E.evaluate type')
      in Ann norm'type (expand'expr ali'env expr)
    Intro name exprs -> Intro name exprs -- NOTE: I can safely do that - this is generated, no type annotations can get there
    Elim cons'decls val destrs -> Elim cons'decls val destrs -- NOTE: I can safely do that - this is generated, no type annotations can get there


{-  This function 

-}
infer'decls :: [Declaration] -> KindEnv -> Either Error KindEnv
infer'decls binds k'env = do
  let 
      (only'aliases, rest) = partition is'alias binds
      -- get all the aliases

  check'for'cycles only'aliases

  let only'types = filter is'type'decl binds
      -- take only type declarations
      -- ted z tech aliasu udelam kontext a normalizuju typy v only'types
      -- NOW, map all DataDecls and TypeAlias -> expand the aliases
      -- expanded = map (expand'aliases $ make'alias'env only'aliases) only'types
      Right expanded = run'norm (make'alias'env only'aliases) (expand'aliases only'types)
      data'pairs = map to'pair expanded

  infer'data k'env data'pairs
  
    where
      is'type'decl :: Declaration -> Bool
      is'type'decl (DataDecl _ _ _) = True
      is'type'decl (TypeAlias _ _) = True
      is'type'decl _ = False

      check'for'cycles :: [Declaration] -> Either Error ()
      check'for'cycles decls = do
        let ds = map to'pair decls
            indexed = index'bindings ds
            graph = build'ali'graph ds indexed
            solved = stronglyConnComp graph
        all'acyclic solved

          where
            -- DUPLICATION
            to'pair :: Declaration -> (String, Type)
            to'pair (TypeAlias name type') = (name, type')

            all'acyclic :: [SCC (String, Type)] -> Either Error ()
            all'acyclic [] = return ()
            all'acyclic ((AcyclicSCC bind) : sccs) =
              all'acyclic sccs
            all'acyclic ((CyclicSCC aliases) : sccs) =
              Left $ SynonymCycle aliases

            build'ali'graph :: [(String, Type)] -> Map.Map String Int -> [((String, Type), Int, [Int])]
            build'ali'graph bindings indexer = graph
              where
                get'deps :: Type -> Set.Set Int
                get'deps expr =
                  case expr of
                    TyVar name -> Set.empty
                    TyCon name -> maybe Set.empty Set.singleton (indexer !? name)
                    TyTuple types -> foldl (\ deps'acc expr -> deps'acc `Set.union` get'deps expr) Set.empty types
                    TyArr t'from t'to -> get'deps t'from `Set.union` get'deps t'to
                    TyApp t'left t'right -> get'deps t'left `Set.union` get'deps t'right
                    TyOp par t' -> get'deps t'

                dependencies = map (get'deps . snd) bindings

                graph = zipWith (\ (name, expr) deps -> ((name, expr), indexer ! name, Set.toList deps)) bindings dependencies


        -- musim sestavit graf
        -- takze nejdriv ocislovat jednotlivy aliasy
        -- pak sestavit graf
        -- pak ho projit a checknout ze nic neni Cyclic


        -- = when (name `occurs'in` type') $ Left $ SynonymCycle name type'
        -- = if name `occurs'in` type'
        --   then Left $ SynonymCycle name
        --   else return ()

      -- DUPLICATION
      expand'aliases :: [Declaration] -> Analize [Declaration]
      expand'aliases [] = return []
      expand'aliases ((DataDecl name params constructors) : decls) = do
        constrs <- mapM expand'constr constructors
        rest <- expand'aliases decls
        return $ DataDecl name params constrs : rest
      expand'aliases ((TypeAlias name type') : decls) = do
        n'type <- E.evaluate type'
        rest <- expand'aliases decls
        return $ TypeAlias name n'type : rest
      -- expand'aliases :: Map.Map String Type -> Declaration -> Declaration
      -- expand'aliases ali'env (DataDecl name params constructors)
      --   = DataDecl name params $ map (expand'constr ali'env) constructors
      -- expand'aliases ali'env (TypeAlias name type')
      --   = TypeAlias name $ N.normalize ali'env type'
      -- expand'aliases _ impossible = impossible

      expand'constr :: ConstrDecl -> Analize ConstrDecl
      expand'constr (ConDecl name param'types) = do
        par't <- mapM E.evaluate param'types
        return $ ConDecl name par't

      -- DUPLICATION
      is'alias (TypeAlias _ _) = True
      is'alias _ = False

      -- DUPLICATION
      make'alias'env decls = Map.fromList $ map (\ (TypeAlias name type') -> (name, type')) decls

      to'pair :: Declaration -> (String, Declaration)
      to'pair d@(DataDecl name _ _) = (name, d)
      to'pair a@(TypeAlias name type') = (name, a)


{-  This function -}
infer'top :: TypeEnv -> [(String, Expression)] -> Either Error TypeEnv
infer'top environment bindings =
  case run'infer'many environment (infer'many bindings) of
    Left err -> Left err
    Right (type'bindings, constraints) -> -- ([(String, Type)], [Constraint])
        case runSolve constraints  of
          Left err -> Left err
          Right subst -> do
            let scheme'bindings = map (second (closeOver . apply subst)) type'bindings
                env' = apply subst $ environment `Map.union` Map.fromList scheme'bindings
            return env'




infer'many :: [(String, Expression)] -> Analize ([(String, Type)], [Constraint Type])
infer'many bindings = do
  let indexed = index'bindings bindings
  let graph = build'graph bindings indexed
  let solved = stronglyConnComp graph
  -- ted to mam vyreseny a co musim udelat je
  -- ze projdu celej ten   list a pro kazdy CyclicSCC [(String, Expression)]
    -- priradim kazdymu jmenu Forall [] <$> fresh
    -- pak vlastne provedu posbirani constraintu
    -- pak je vratim nekam
  -- pro kazdy AcyclicSCC (String, Expression)
    -- tady to Expression nezavisi ani samo na sobe, takze neni potreba to zanaset
    -- jenom to infernu -> posbiram constrainty a type a vratim je nekam vejs
  infer'groups solved
    where
      infer'groups :: [SCC (String, Expression)] -> Analize ([(String, Type)], [Constraint Type])
      infer'groups [] = return ([], [])
      infer'groups ((AcyclicSCC bind) : sccs) = do
        (t'binds, constrs) <- infer'group [bind]
        t'env <- ask
        (t'binds', constrs') <- merge'into'env (map (\ (n, t) -> (n, generalize t'env t)) t'binds) $ infer'groups sccs
        return (t'binds ++ t'binds', constrs ++ constrs')

      infer'groups ((CyclicSCC bindings) : sccs) = do
        (t'binds, constrs) <- infer'group bindings
        t'env <- ask
        (t'binds', constrs') <- merge'into'env (map (\ (n, t) -> (n, generalize t'env t)) t'binds) $ infer'groups sccs
        return (t'binds ++ t'binds', constrs ++ constrs')


infer'group :: [(String, Expression)] -> Analize ([(String, Type)], [Constraint Type])
infer'group bindings = do
  let names = map fst bindings
      gener name = do ForAll [] <$> fresh
  fresh'vars <- mapM gener names
  merge'into'env (zip names fresh'vars) $ infer'many' bindings


index'bindings :: [(String, a)] -> Map.Map String Int -- [((String, Expression), Int)]
index'bindings = enumerate'bindings 0
  where
    enumerate'bindings :: Int -> [(String, a)] -> Map.Map String Int
    enumerate'bindings _ [] = Map.empty
    enumerate'bindings n ((name, expr) : bs) = Map.insert name n $ enumerate'bindings (n + 1) bs


build'graph :: [(String, Expression)] -> Map.Map String Int -> [((String, Expression), Int, [Int])]
build'graph bindings indexer = graph
  where
    get'deps :: Expression -> Set.Set Int
    get'deps expr =
      case expr of
        Var name ->
          maybe Set.empty Set.singleton (indexer !? name)

        Op _ -> Set.empty

        Lit _ -> Set.empty

        Lam par body ->
          case indexer !? par of
            Nothing -> get'deps body
            Just ix -> Set.delete ix $ get'deps body

        App left right ->
          get'deps left `Set.union` get'deps right

        Tuple exprs ->
          foldl (\ deps'acc expr -> deps'acc `Set.union` get'deps expr) Set.empty exprs

        If b'expr then'expr else'expr ->
          let b'deps = get'deps b'expr
              t'deps = get'deps then'expr
              e'deps = get'deps else'expr
          in  b'deps `Set.union` t'deps `Set.union` e'deps

        Let name val'expr body'expr ->
          let v'deps = get'deps val'expr
              b'deps = get'deps body'expr
          in  v'deps `Set.union` b'deps

        Fix expr -> get'deps expr

        -- this should always yield an empty Set, but just to be sure
        Intro _ exprs -> foldl (\ deps'acc expr -> deps'acc `Set.union` get'deps expr) Set.empty exprs

        Elim _ expr exprs ->
          foldl (\ deps'acc expr -> deps'acc `Set.union` get'deps expr) Set.empty (expr : exprs)

        Ann _ expr ->
          get'deps expr


    dependencies = map (get'deps . snd) bindings

    graph = zipWith (\ (name, expr) deps -> ((name, expr), indexer ! name, Set.toList deps)) bindings dependencies


infer'many' :: [(String, Expression)] -> Analize ([(String, Type)], [Constraint Type])
infer'many' [] = do
  return ([], [])
infer'many' ((name, expr) : exprs) = do
  (type', constraints) <- infer expr

  orig'type <- lookup'env name
  (types, constrs') <- infer'many' exprs
  return ((name, type') : types, (orig'type, type') : constraints ++ constrs')
  
  -- this should actually work
  -- instantiate should do nothing to the fresh type variable because the ForAll
  -- has an empty list of type parameters
