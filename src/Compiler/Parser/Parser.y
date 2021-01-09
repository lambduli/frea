{
module Compiler.Parser.Parser (parse'expr) where

import Control.Monad (unless, fail)
import Control.Monad.State

import Compiler.Parser.Token
import Compiler.Parser.Lexer
import Compiler.Parser.Utils


import Compiler.Syntax.Bind
import Compiler.Syntax.Declaration
import Compiler.Syntax.Expression
import Compiler.Syntax.Literal
import Compiler.Syntax.MatchGroup
import Compiler.Syntax.Pattern
import Compiler.Syntax.Signature
import Compiler.Syntax.Type
}


%name parserAct
%tokentype { Token }
%error { parseError }
%monad { P }
%lexer { lexer } { TokEOF }
%expect 0


%token
  match         { TokMatch }
  with          { TokWith }
  data          { TokData }
  if            { TokIf }
  then          { TokThen }
  else          { TokElse }
  let           { TokLet }
  in            { TokIn }
  type          { TokType }
  '_'           { TokHole }
  the           { TokThe }
  claim         { TokClaim }
  define        { TokDefine }
  lambda        { TokLambda }
  fix           { TokFix }
  

  '-'           { TokVarId "-" }
  '->'          { TokVarId "->" }
  '='           { TokVarId "=" }


  varid         { TokVarId $$ }
  symid         { TokNativeSym $$ }


  '('           { TokLeftParen }
  ')'           { TokRightParen }
  '['           { TokLeftBracket }
  ']'           { TokRightBracket }
  ','           { TokComma }


  unit          { TokUnit }
  integer       { TokInt $$ }
  char          { TokChar $$ }
  double        { TokDouble $$ }
  string        { TokString $$ }
  bool          { TokBool $$ }

%%
Program         ::  { Expression }
                :   Exp                                             { $1 }

Params          ::  { [String] }
                :   NoneOrMany(Var)                                 { $1 }

Var             ::  { String }
                :   varid                                           { $1 }

Op              ::  { String }
                :   symid                                           { $1 }

Exp             ::  { Expression }
                :   Var                                             { Var $1 }
                |   Op                                              { Op $1 }
                |   Lit                                             { Lit $1 }
                |   lambda Params '->' Exp                          { foldr (\ arg body -> Lam arg body) $4 $2 }

                |   '(' Exp OneOrMany(Exp) ')'                      { foldl App $2 $3 }
                --  NOTE: what about (fn) ? you can't call a function without arguments!
                |   '(' Exp ')'                                     { $2 }
                --  NOTE: therefore redundant parentheses are a OK

                |   fix Exp                                         { Fix $2 }
                |   if Exp then Exp else Exp                        { If $2 $4 $6 }
                |   let Var '=' Exp in Exp                          { Let $2 $4 $6 }
                |   '(' Exp CommaSeparated(Exp) ')'                 { Tuple $ $2 : $3 }
                |   '[' NoneOrManySeparated(Exp) ']'                { List $2 }

Lit             ::  { Lit }
                :   Integer                                         { $1 }
                |   Double                                          { $1 }
                |   char                                            { LitChar $1 }
                |   string                                          { LitString $1 }
                |   bool                                            { LitBool $1 }
                |   unit                                            { LitUnit }

Integer         ::  { Lit }
                :   integer                                         { LitInt $1 }

Double          ::  { Lit }
                :   double                                          { LitDouble $1 }

NoneOrMany(tok)
                :   {- empty -}                                     { [] }
                |   NoneOrMany(tok) tok                             { $1 ++ [$2] }

OneOrMany(tok)
                :   tok NoneOrMany(tok)                             { $1 : $2 }

CommaSeparated(tok)
                :   ',' tok                                         { [$2] }
                |   ',' tok CommaSeparated(tok)                     { $2 : $3 }

NoneOrManySeparated(tok)
                :   {- empty -}                                     { [] }
                |   tok                                             { [$1] }
                |   tok ',' NoneOrManySeparated(tok)                { $1 : $3 }

{

parseError _ = do
  lno <- getLineNo
  colno <- getColNo
  error $ "Parse error on line " ++ show lno ++ ", column " ++ show colno ++ "."


parse'expr :: String -> Expression
parse'expr s =
  evalP parserAct s
}