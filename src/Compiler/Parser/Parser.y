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
  else          { TokElse }
  if            { TokIf }
  then          { TokThen }
  let           { TokLet }
  in            { TokIn }
  type          { TokType }
  '_'           { TokHole }
  the           { TokThe }
  claim         { TokClaim }
  define        { TokDefine }
  lambda        { TokLambda }
  

  '-'           { TokVarId "-" }
  '->'          { TokVarId "->" }


  varid         { TokVarId $$ }
  conid         { TokConstrId $$ }
  symid         { TokNativeSym $$ }



  '('           { TokLeftParen }
  ')'           { TokRightParen }
  ','           { TokComma }


  integer       { TokInt $$ }
  char          { TokChar $$ }
  double        { TokDouble $$ }
  string        { TokString $$ }

%%
Program         ::  { Expression }
                :   Exp                                             { $1 }

--Program         ::  { [Declaration] }
--                :   NoneOrMany(TopDecl)                             { $1 }

TopDecl         ::  { Declaration }
                :   TypeDecl                                        { $1 }
                |   DataDecl                                        { $1 }
                |   Decl                                            { $1 }

TypeDecl        ::  { Declaration }
                :   '(' type '(' Con NoneOrMany(Var) ')' Type ')'   { TypeDecl $4 $5 $7 }
                |   '(' type Con Type ')'                           { TypeDecl $3 [] $4 }

DataDecl        ::  { Declaration }
                :   '(' data '(' Con NoneOrMany(Var) ')' NoneOrMany(Constr) ')'
                                                                    { DataDecl $4 $5 $7 }
                |   '(' data Con NoneOrMany(Constr) ')'             { DataDecl $3 [] $4 }

Constr          ::  { ConstrDecl }
                :   '(' Con NoneOrMany(Type) ')'                    { ConstrDecl $2 $3 }

Decl            ::  { Declaration }
                :   TypeSignDecl                                    { $1 }
                |   FunDecl                                         { $1 }

TypeSignDecl    ::  { Declaration }
                :   '(' claim Con Type ')'                          { Signature $ TypeSig $3 $4 }

FunDecl         ::  { Declaration }
                :   '(' define Var Exp ')'                          { Binding $3 $4 }

Type            ::  { Type }
                :   Var                                             { TyVar $1 }
                |   Con                                             { TyCon $1 }
                -- |   '(' Type OneOrMany(Type) ')'                    { foldl AppTy $2 $3 }
                |   '(' '->' Type Type ')'                          { TyArr $3 $4 }
                |   '(' Type ')'                                    { $2 }
                -- TODO: what about built-in constructors?

Params          ::  { [String] }
                :   NoneOrMany(Var)                                 { $1 }

Var             ::  { String }
                :   varid                                           { $1 }

Con             ::  { String }
                :   conid                                           { $1 }

Op              ::  { String }
                :   symid                                           { $1 }

Exp             ::  { Expression }
                :   Var                                             { Var $1 }
                |   Op                                              { Op $1 }
                |   Con                                             { Con $1 }
                |   Lit                                             { Lit $1 }
                |   '(' lambda '(' Params ')' Exp ')'               { foldr (\ arg body -> Lam arg body) $6 $4 }

                |   '(' Exp OneOrMany(Exp) ')'                      { foldl App $2 $3 }
                -- TODO: what about (fn) ? you can't call a function without arguments!
                |   '(' Exp ')'                                     { $2 }
                -- therefore redundant parentheses are a OK

                |   '-' Exp                                         { NegApp $2 }
                |   '(' match Exp with MatchOptions ')'             { MatchWith $3 $5 }
                |   '(' if Exp Exp Exp ')'                          { If $3 $4 $5 }
                |   '(' let '(' Var Exp ')' in Exp ')'              { Let $4 $5 $8 }
                --  (let (foo (+ 23 23)) in (+ foo foo))
                |   '(' the Type Exp ')'                            { Typed $3 $4 }

MatchOptions    ::  { MatchGroup }
                :   '(' NoneOrMany(Match) ')'                       { MG $2 }

Match           ::  { Match }
                :   '(' Pattern Exp ')'                             { Match
                                                                        { matchPat = $2
                                                                        , rhs = $3 }
                                                                    }

Pattern         ::  { Pattern }
                :   Var                                             { PatVar $1 }
                |   Con                                             { PatCon $1 [] }
                |   '(' Con NoneOrMany(Pattern) ')'                 { PatCon $2 $3 }
                |   Lit                                             { PatLit $1 }
                |   '_'                                             { PatWild }
                |   '-' Double                                      { PatLit $2 }
                |   '-' Integer                                     { PatLit $2 }

Lit             ::  { Lit }
                :   Integer                                         { $1 }
                |   Double                                          { $1 }
                |   char                                            { LitChar $1 }
                |   string                                          { LitString $1 }

Integer         ::  { Lit }
                :   integer                                         { LitInt $1 }

Double          ::  { Lit }
                :   double                                          { LitDouble $1 }

NoneOrMany(tok)
                :   {- empty -}                                     { [] }
                |   NoneOrMany(tok) tok                             { $1 ++ [$2] }

OneOrMany(tok)
                :   tok NoneOrMany(tok)                             { $1 : $2 }



{

-- TODO: in order for this function to be able to do better job at reporting problems
-- I suppose I need to add some stuff into ParseState type
-- I could probably also keep much bigger part of the code - block or something like that
-- than later when searching for the right representation of the error I should have all important info

parseError _ = do
  lno <- getLineNo
  s <- get
  error $ "Parse error on line " ++ show lno ++ "\n\n" ++ show s



-- TODO: in order to be able to produce either Result or Raise a Lexical/Syntactical error
-- I will probably need to modify the monad type P
-- create something which will be able to have either OK state or Error state
-- https://www.haskell.org/happy/doc/html/sec-monads.html#sec-exception
-- section 2.5.1. Handling Parse Errors
-- %monad { E } { thenE } { returnE }
-- thenE is >>=
-- not sure what is the second one - something pure for Applicative
-- it is probably `return`


--parse :: String -> [Declaration] -- TODO: change later
--parse s =
--  evalP parserAct s

parse'expr :: String -> Expression -- TODO: change later
parse'expr s =
  evalP parserAct s
}