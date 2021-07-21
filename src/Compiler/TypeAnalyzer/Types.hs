module Compiler.TypeAnalyzer.Types where


import Compiler.Syntax (Type(..))

import Compiler.Syntax.Kind


-- | Assuming that Bool will be defined in the Prelude.
t'Bool = TyCon "Bool" Star

t'Int = TyCon "Int" Star

t'Double = TyCon "Double" Star

t'Char = TyCon "Char" Star
