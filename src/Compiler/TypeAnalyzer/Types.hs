module Compiler.TypeAnalyzer.Types where


import Compiler.Syntax.Type

import Compiler.Syntax.Kind


-- | Assuming that Bool will be defined in the Prelude.
t'Bool = TyCon $ TCon "Bool" Star

t'Int = TyCon $ TCon "Int" Star

t'Double = TyCon $ TCon "Double" Star

t'Char = TyCon $ TCon "Char" Star
