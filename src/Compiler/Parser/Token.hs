module Compiler.Parser.Token where


data TokPosition
  = TokPosition { line :: Int
                , column :: Int }
  deriving (Eq, Show)


data Token
  -- reserved identifiers/words
  = TokMatch
  | TokWith
  | TokData -- data
  | TokEliminator String -- which-_
  | TokIf -- if
  | TokThen -- then
  | TokElse -- else
  | TokLet  -- let
  | TokIn -- in

  | TokType -- type
  | TokHole -- _
  | TokThe
  | TokClaim -- type annotations
  | TokDefine
  | TokLambda -- lambda
  | TokFix -- fix
  | TokLetrec -- letrec
  

  -- variables
  | TokVarUpper String
  | TokVarLower String
  -- | TokConstrId String
  | TokNativeSym String
  | TokOperator String
  | TokOpConstr String
  

  -- special symbols
  | TokLeftParen -- (
  | TokRightParen -- )
  | TokLeftBracket -- [
  | TokRightBracket -- ]
  | TokComma  -- ,
  | TokBackTick -- `
  | TokSemicolon -- ;
  | TokLeftBrace -- {
  | TokRightBrace -- }
  | TokHasType -- ::


  -- literals
  | TokInt Int -- 23
  | TokChar Char -- 'a'
  | TokDouble Double -- 0.23
  | TokString String -- "string"


  | TokAssume -- assume
  | TokRec    -- rec

  | TokEOF
  deriving (Eq, Show)
