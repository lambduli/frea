{
module Compiler.Parser.Parser (parse'expr, parse'type) where

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


%name parserMain
%name parserType Type
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
  '|'           { TokOperator "|" }
  '::'          { TokHasType }

  varid         { TokVarLower $$ }
  conid         { TokVarUpper $$ }
  op            { TokOperator $$ }
  opcon         { TokOpConstr $$ }
  symid         { TokNativeSym $$ }
  elim          { TokEliminator $$ }


  '('           { TokLeftParen }
  ')'           { TokRightParen }
  '['           { TokLeftBracket }
  ']'           { TokRightBracket }
  ','           { TokComma }
  '`'           { TokBackTick }
  '{'           { TokLeftBrace }
  '}'           { TokRightBrace }
  ';'           { TokSemicolon }


  unit          { TokVarUpper "()" }
  integer       { TokInt $$ }
  char          { TokChar $$ }
  double        { TokDouble $$ }
  string        { TokString $$ }

  assume        { TokAssume }
  rec           { TokRec }

  module        { TokModule }
  where         { TokWhere }

%%
Program         ::  { Either [Declaration] Expression }
                :   Exp                                             { Right $1 }
                |   Module                                          { Left $1 }

Module          ::  { [Declaration] }
                :   module UpIdent where Layout(Declaration)        { $4 }

Declaration     ::  { Declaration }
                :   Fun                                             { $1 }
                |   Data                                            { $1 }
                |   TypeSynonym                                     { $1 }

Data            ::  { Declaration }
                :   data UpIdent Params Constructors                { DataDecl $2 $3 $4 }

Constructors    ::  { [ConstrDecl] }
                :   {- empty -}                                     { [] }
                |   '=' Constr NoneOrMany(ConstrOther)              { $2 : $3 }

ConName         ::  { String }
                :   OpCon                                           { $1 }
                |   '`' Con '`'                                     { $2 }

Constr          ::  { ConstrDecl }
                :   UpIdent NoneOrMany(TyAppLeft)                  { ConDecl $1 $2 }
                |   TyAppLeft ConName OneOrMany(TyAppLeft)        { ConDecl $2 ($1 : $3) }

ConstrOther     ::  { ConstrDecl }
                :   '|' Constr                                      { $2 }

TypeSynonym     ::  { Declaration }
                :   type Con NoneOrMany(Var) '=' Type                { TypeAlias $2 (foldr TyOp $5 $3) }

Params          ::  { [String] }
                :   NoneOrMany(Var)                                 { $1 }

LowIdent        ::  { String }
                :   Var                                             { $1 }
                |   '(' Op ')'                                      { $2 }

UpIdent         ::  { String }
                :   Con                                             { $1 }
                |   '(' opcon ')'                                   { $2 }
                |   unit                                            { "()" }

Var             ::  { String }
                :   varid                                           { $1 }

Con             ::  { String }
                :   conid                                           { $1 }

Op              ::  { String }
                :   op                                              { $1 }

OpCon           ::  { String }
                :   opcon                                           { $1 }

Oper            ::  { Expression }
                :   op                                              { Var $1 }
                |   opcon                                           { Var $1 }

Exp             ::  { Expression }
                :   AppLeft                                         { $1 }
                |   Application                                     { $1 }
                |   '(' Exp ')'                                     { $2 }

Application     ::  { Expression }
                :   AppLeft OneOrMany(AppRight)                     { foldl App $1 $2 }
                |   AppLeft '`' Var '`' OneOrMany(AppRight)         { foldl App (Var $3) ($1 : $5) }
                |   AppLeft '`' Con '`' OneOrMany(AppRight)         { foldl App (Var $3) ($1 : $5) }
                --  Note: Consider adding Constructor Expression for this ^^^
                |   AppLeft Oper OneOrMany(AppRight)                { foldl App $2 ($1 : $3) }
                --  NOTE: what about (fn) ? you can't call a function without arguments!

AppLeft         ::  { Expression }
                :   LowIdent                                        { Var $1 }
                |   Con                                             { Var $1 }
                |   '(' opcon ')'                                   { Var $2 }
                |   symid                                           { Op $1 }
                |   Lit                                             { $1 }
                |   lambda Params '->' Exp                          { foldr (\ arg body -> Lam arg body) $4 $2 }
                |   '(' AppLeft ')'                                 { $2 }
                                |   fix Exp                                         { Fix $2 }
                |   if Exp then Exp else Exp                        { If $2 $4 $6 }
                |   let Layout(Binding) in Exp                      { foldr
                                                                        (\ (name, expr) body -> Let name expr body)
                                                                        $4
                                                                        $2 }
                |   letrec LowIdent Params '=' Exp in Exp           { Let $2 (Fix $ foldr (\ arg body -> Lam arg body) $5 ($2 : $3)) $7 }
                -- TODO: do the same for letrec
                |   '(' Exp CommaSeparated(Exp) ')'                 { Tuple $ $2 : $3 }
                |   '[' NoneOrManySeparated(Exp) ']'                 { foldr (\ item acc -> App (App (Var ":") item) acc ) (Var "[]") $2 }
                -- wiring the List type into the compiler
                |   '(' Application ')'                             { $2 }
                |   '(' Exp '::' Type ')'                           { Ann $4 $2 }

AppRight        ::  { Expression }
                :   AppLeft                                         { $1 }


Binding         ::  { (String, Expression) }
                :   LowIdent Params '=' Exp                         { ($1, foldr (\ arg body -> Lam arg body) $4 $2) }
                |   Var Op Params '=' Exp                           { ($2, foldr Lam $5 ($1 : $3)) }
                |   Var '`' Var '`' Params '=' Exp                  { ($3, foldr Lam $7 ($1 : $5)) }
                
                |   rec LowIdent Params '=' Exp                     { ($2, Fix $ foldr (\ arg body -> Lam arg body) $5 ($2 : $3)) }
                |   rec Var Op Params '=' Exp                       { ($3, Fix $ foldr Lam $6 ($3 : $2 : $4)) }
                |   rec Var '`' Var '`' Params '=' Exp              { ($4, Fix $ foldr Lam $8 ($4 : $2 : $6)) }

GlobalBinding   ::  { (String, Expression) }
                :   LowIdent Params '=' Exp                         { ($1, foldr (\ arg body -> Lam arg body) $4 $2) }
                |   Var Op Params '=' Exp                           { ($2, foldr Lam $5 ($1 : $3)) }
                |   Var '`' Var '`' Params '=' Exp                  { ($3, foldr Lam $7 ($1 : $5)) }

Fun             ::  { Declaration }
                :   GlobalBinding                                   { Binding (fst $1) (snd $1) }
                |   Annotation ';' GlobalBinding                    { Annotated (fst $3) (snd $1) (snd $3) }

Lit             ::  { Expression }
                :   Integer                                         { Lit $1 }
                |   Double                                          { Lit $1 }
                |   char                                            { Lit $ LitChar $1 }
                |   string                                          { foldr (\ item acc -> App (App (Var ":") (Lit $ LitChar item)) acc ) (Var "[]") $1 }

Integer         ::  { Lit }
                :   integer                                         { LitInt $1 }

Double          ::  { Lit }
                :   double                                          { LitDouble $1 }

Annotation      ::  { (String, Type) }
                :   LowIdent '::' Type                              { ($1, $3) }
                -- |   '(' Op ')' '::' Type                            { ($2, $5) }

Type            ::  { Type }
                :   TyAppLeft                                       { $1 }
                |   TyApp                                           { $1 }
                |   '(' Type ')'                                    { $2 }

TyArr           ::  { Type }
                :   Type '->' Type                                  { TyArr $1 $3 }

TyTuple         ::  { Type }
                :   '(' Type CommaSeparated(Type) ')'               { TyTuple $ $2 : $3 }

TyApp           ::  { Type }
                :   TyAppLeft OneOrMany(TyAppLeft)                 { foldl TyApp $1 $2 }

TyAppLeft       ::  { Type }
                :   LowIdent                                        { TyVar $1 }
                |   UpIdent                                         { TyCon $1 }
                |   TyArr                                           { $1 }
                |   TyTuple                                         { $1 }
                |   '(' TyApp ')'                                   { $2 }
                |   '(' TyAppLeft ')'                               { $2 }

-- TyAppLeft      ::  { Type }
--                 :   TyAppLeft                                       { $1 }

NoneOrMany(tok)
                :   {- empty -}                                     { [] }
                |   tok NoneOrMany(tok)                             { $1 : $2 }

Layout(tok)
                :   '{' LayoutInside(tok)                           { $2 }

LayoutInside(tok)
                :   tok '}'                                         { [$1] }
                |   tok ';' LayoutInside(tok)                       { $1 : $3 }

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


parse'expr :: String -> Either [Declaration] Expression
parse'expr s = evalP parserMain s


parse'type :: String -> Type
parse'type s = evalP parserType s

}
