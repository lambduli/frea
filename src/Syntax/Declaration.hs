{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Syntax.Declaration where

import Syntax (Sig, Type, Expression)


data Declaration
  = Binding String Expression       -- (define ...)
  | Signature Sig                   -- Type signature
  | DataDecl String [String] [ConstrDecl] -- Data type declaration -- name type'params list'of'consturctors'with'params
  | TypeDecl String [String] Type        -- Type declaration -- only single constructor -- basically alias


data ConstrDecl
  = ConstrDecl String [Type]
  | ConstrFieldDecl String [(String, Type)] -- not using this one so far
