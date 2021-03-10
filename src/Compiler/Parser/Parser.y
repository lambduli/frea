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

import Interpreter.Command
}


%name parserAct
%tokentype { Token }
%error { parseError }
%monad { P }
%lexer { lexer } { TokEOF }
--%expect 0


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
  letrec        { TokLetrec }
  

  '->'          { TokOperator "->" }
  '='           { TokOperator "=" }


  varid         { TokVarId $$ }
  op            { TokOperator $$ }
  symid         { TokNativeSym $$ }


  '('           { TokLeftParen }
  ')'           { TokRightParen }
  '['           { TokLeftBracket }
  ']'           { TokRightBracket }
  ','           { TokComma }
  '`'           { TokBackTick }


  unit          { TokUnit }
  integer       { TokInt $$ }
  char          { TokChar $$ }
  double        { TokDouble $$ }
  string        { TokString $$ }
  bool          { TokBool $$ }

  assume        { TokAssume }
  rec           { TokRec }

%%
Program         ::  { Either Command Expression }
                :   Exp                                             { Right $1 }
                |   Assume                                          { Left $1 }

Assume          ::  { Command }
                :   assume OneOrMany(Assumption)                    { Assume $2 }

Assumption      ::  { (String, Expression) }
                :   Ident '=' Exp                                   { ($1, $3) }
                |   Ident Params '=' Exp                            { ($1, foldr (\ arg body -> Lam arg body) $4 $2) }
                |   rec Ident Params '=' Exp                        { ($2, Fix $ foldr (\ arg body -> Lam arg body) $5 ($2 : $3)) }

Params          ::  { [String] }
                :   NoneOrMany(Var)                                 { $1 }

Ident           ::  { String }
                :   Var                                             { $1 }
                |   '(' Op ')'                                      { $2 }

Var             ::  { String }
                :   varid                                           { $1 }

Op              ::  { String }
                :   symid                                           { $1 }
                |   op                                              { $1 }

Oper            ::  { Expression }
                :   symid                                           { Op $1 }
                |   op                                              { Var $1 }


Exp             ::  { Expression }
                :   Var                                             { Var $1 }
                |   '(' Oper ')'                                    { $2 }
                |   Lit                                             { Lit $1 }
                |   lambda Params '->' Exp                          { foldr (\ arg body -> Lam arg body) $4 $2 }

                |   Exp '`' Var '`' Exp                             { App (App (Var $3) $1) $5 }
                |   Exp Oper Exp                                    { App (App $2 $1) $3 }
                |   '(' Exp OneOrMany(Exp) ')'                      { foldl App $2 $3 }
                --  NOTE: what about (fn) ? you can't call a function without arguments!
                |   '(' Exp ')'                                     { $2 }
                --  NOTE: therefore redundant parentheses are a OK

                |   fix Exp                                         { Fix $2 }
                |   if Exp then Exp else Exp                        { If $2 $4 $6 }
                |   let OneOrMany(Binding) in Exp                   { foldr
                                                                        (\ (name, expr) body -> Let name expr body)
                                                                        $4
                                                                        $2 }
                |   letrec Ident Params '=' Exp in Exp              { Let $2 (Fix $ foldr (\ arg body -> Lam arg body) $5 ($2 : $3)) $7 }
                -- TODO: do the same for letrec
                |   '(' Exp CommaSeparated(Exp) ')'                 { Tuple $ $2 : $3 }
                |   '[' NoneOrManySeparated(Exp) ']'                { List $2 }

Binding         ::  { (String, Expression) }
                :   Ident '=' Exp                                   { ($1, $3) }
                |   Ident Params '=' Exp                            { ($1, foldr (\ arg body -> Lam arg body) $4 $2) }
                |   rec Ident Params '=' Exp                        { ($2, Fix $ foldr (\ arg body -> Lam arg body) $5 ($2 : $3)) }

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
                |   tok NoneOrMany(tok)                             { $1 : $2 }

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
  s <- get
  error $ "Parse error on line " ++ show lno ++ ", column " ++ show colno ++ "." ++ "  " ++ show s


parse'expr :: String -> Either Command Expression
parse'expr s =
  evalP parserAct s
}