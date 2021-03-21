module Compiler.TypeChecker.Type where


import Compiler.Syntax ( Type(..), Scheme(..))


t'Bool = TyCon "Bool"

t'Int = TyCon "Int"

t'Double = TyCon "Double"

t'Char = TyCon "Char"

t'Unit = TyCon "Unit"