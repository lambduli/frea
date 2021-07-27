module Compiler.TypeAnalyzer.Class where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Type

import Compiler.TypeAnalyzer.Types


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

type EnvTransformer = ClassEnv -> Maybe ClassEnv

