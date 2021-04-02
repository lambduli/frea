module Compiler.KindChecker.KindEnv where


import qualified Data.Map.Strict as Map
import Compiler.Syntax.Kind


type KindEnv = Map.Map String Kind


empty'k'env :: KindEnv
empty'k'env = Map.fromList
  [ ("Bool"   , Star) 
  , ("Int"    , Star) 
  , ("Double" , Star) 
  , ("Char"   , Star) 
  , ("Unit"   , Star) ]
  -- Not including a List or Tuple
  -- that's because those are not really a constructors of proper types
