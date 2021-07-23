module Compiler.TypeAnalyzer.Types where


import Compiler.Syntax.Type

import Compiler.Syntax.Kind


t'Bool, t'Int, t'Double, t'Char, t'Arr :: Type
-- | Assuming that Bool will be defined in the Prelude.
t'Bool = TyCon $ TCon "Bool" Star

t'Int = TyCon $ TCon "Int" Star

t'Double = TyCon $ TCon "Double" Star

t'Char = TyCon $ TCon "Char" Star

t'Arr = TyCon $ TCon "(->)" (Star `KArr` (Star `KArr` Star))


infixr 4 `type'fn`
type'fn :: Type -> Type -> Type
domain `type'fn` codomain = TyApp (TyApp t'Arr domain) codomain
