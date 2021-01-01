module Compiler.Syntax.Type where

-- I am gonna need to implement AST FIX similar to the one in the Transform
-- I should probably move the logic from the JS Syntax tree to Gliss AST
-- and also add a fix, which will transform VarTy such as Int or Bool (if it's declared by user)
-- to ConTy
--  
data Type
  = VarTy String
  | ConTy String
  | BuiltInTyCon BuiltInTyCon
  | AppTy Type Type
  | FunTy Type Type
  -- | Forall String Type -- type level lambda

data TypeScheme
  = ForAll [String] Type

data BuiltInTyCon
  = UnitTyCon
  | FunTyCon