module Compiler.TypeChecker.Constraint where

import Compiler.Syntax.Type
import Compiler.Syntax.Kind


type Constraint a = (a, a)
