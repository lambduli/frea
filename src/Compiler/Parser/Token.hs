module Compiler.Parser.Token where


data TokPosition
  = TokPosition { line :: Int
                , column :: Int }
  deriving (Eq, Show)


-- TODO: walk through each Token Constructor and remove all which are not needed

data Token
  -- reserved identifiers/words
  = TokMatch
  | TokWith
  | TokData -- data
  -- | TokDefault
  -- | TokDeriving
  -- | TokDo
  | TokIf
  | TokThen
  | TokElse
  | TokLet  -- let
  | TokIn -- in

  | TokType -- type
  | TokHole -- _
  | TokThe
  | TokClaim -- type annotations
  | TokDefine
  | TokLambda -- lambda
  

  -- variables
  | TokVarId String
  | TokConstrId String
  | TokNativeSym String
  

  -- special symbols
  | TokLeftParen -- (
  | TokRightParen -- )
  | TokComma  -- ,


  -- literals
  | TokInt Int -- 23
  | TokChar Char -- 'a'
  | TokDouble Double -- 0.23
  | TokString String -- "string"


  | TokEOF
  deriving (Eq, Show)
