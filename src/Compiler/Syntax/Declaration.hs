{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Compiler.Syntax.Declaration where

import Data.List

import Compiler.Syntax (Sig, Type, Expression)


data Declaration
  = Binding String Expression         -- (define ...)
  | Annotated String Type Expression  -- annotated function declaration
  | Signature Sig                     -- Type signature
  | DataDecl String [String] [ConstrDecl] -- Data type declaration -- name type'params list'of'consturctors'with'params
  | TypeDecl String [String] Type        -- Type declaration -- only single constructor -- basically alias


instance Show Declaration where
  show (Binding name expr) = name ++ " = " ++ show expr
  show (Signature sig) = "<|signature|>"
  show (DataDecl name params constrs)
    = "data " ++ name ++ " " ++ unwords params ++ " = " ++ intercalate " | " (map show constrs)
  show (TypeDecl name params type') = "<|typedecl|>"


data ConstrDecl
  = ConDecl String [Type]
  | ConFieldDecl String [(String, Type)] -- not using this one so far
  deriving (Eq)


instance Show ConstrDecl where
  show (ConDecl name types) = name ++ " " ++ unwords (map show types)
  show (ConFieldDecl name pairs) = "<|confielddecl|>"
