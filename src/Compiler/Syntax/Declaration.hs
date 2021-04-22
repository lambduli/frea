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
  | TypeAlias String Type             -- type String = List Char
  | TypeFun String [String] Type      -- Type operator/function


instance Show Declaration where
  show (Binding name expr)
    = name ++ " = " ++ show expr
  show (Annotated name type' expr)
    = name ++ " :: " ++ show type' ++ "\n" ++ name ++ " = " ++ show expr
  show (Signature sig)
    = show sig
  show (DataDecl name params constrs)
    = "data " ++ name ++ " " ++ unwords params ++ " = " ++ intercalate " | " (map show constrs)
  show (TypeAlias name type')
    = "type " ++ name ++ " = " ++ show type'
  show (TypeFun name params type')
    = "type " ++ name ++ intercalate " " params ++ " = " ++ show type'


data ConstrDecl
  = ConDecl String [Type]
  | ConFieldDecl String [(String, Type)] -- not using this one so far
  deriving (Eq)


instance Show ConstrDecl where
  show (ConDecl name types) = name ++ " " ++ unwords (map show types)
  show (ConFieldDecl name pairs) = "<|confielddecl|>"
