module Compiler.TypeChecker.Error where


data Error a
  = Occurs a a
  | UnifMismatch a a
  | UnboundVar a  
  | UnifShapeMismatch a a
  | UnifCountMismatch [a] [a]
  | SynonymCycle [(String, Type)]

  | Unexpected String