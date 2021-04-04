module Compiler.TypeChecker.Inference.TypeEnv where


import qualified Data.Map.Strict as Map
import Compiler.Syntax.Type
import Compiler.TypeChecker.Type


type TypeEnv = Map.Map String Scheme


-- not really empty
empty't'env :: TypeEnv
empty't'env = Map.fromList
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
  , ("#show", ForAll ["a"]      (TyVar "a" `TyArr` (TyList t'Char)))
  -- , ("#showint", ForAll []      ((TyCon "Int") `TyArr` (TyList (TyCon "Char"))))
  -- , ("#showbool", ForAll []     ((TyCon "Bool") `TyArr` (TyList (TyCon "Char"))))
  -- , ("#showdouble", ForAll []   ((TyCon "Double") `TyArr` (TyList (TyCon "Char"))))
  -- , ("#showchar", ForAll []     ((TyCon "Char") `TyArr` (TyList (TyCon "Char"))))
  -- , ("#showstring", ForAll []   ((TyList $ TyCon "Char") `TyArr` (TyList (TyCon "Char"))))
  -- , ("#showunit", ForAll []     ((TyCon "Unit") `TyArr` (TyList (TyCon "Char"))))
  ]