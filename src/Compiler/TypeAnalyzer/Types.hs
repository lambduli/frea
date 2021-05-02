module Compiler.TypeAnalyzer.Types where


import Compiler.Syntax (Type(..))


-- | Assuming that Bool will be defined in the Prelude.
t'Bool = TyCon "Bool"

t'Int = TyCon "Int"

t'Double = TyCon "Double"

t'Char = TyCon "Char"
