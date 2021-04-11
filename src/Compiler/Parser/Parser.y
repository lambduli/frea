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


  unit          { TokVarUpper "()" }
  integer       { TokInt $$ }
  char          { TokChar $$ }
  double        { TokDouble $$ }
  string        { TokString $$ }

  assume        { TokAssume }
  rec           { TokRec }

%%
Program         ::  { Either [Declaration] Expression }
                :   Exp                                             { Right $1 }
                |   Declarations                                    { Left $1 }

Declarations    ::  { [Declaration] }
                :   OneOrMany(Declaration)                          { $1 }

Declaration     ::  { Declaration }
                :   Decl                                            { $1 }
                |   Data                                            { $1 }

Data            ::  { Declaration }
                :   data UpIdent Params Constructors                { DataDecl $2 $3 $4 }

Constructors    ::  { [ConstrDecl] }
                :   {- empty -}                                     { [] }
                |   '=' Constr NoneOrMany(ConstrOther)              { $2 : $3 }

Constr          ::  { ConstrDecl }
                :   UpIdent NoneOrMany(Type)                        { ConDecl $1 $2 }
                -- |   '(' Op ')' NoneOrMany(Type)                     { ConDecl $2 $4 }
                |   Type OpCon Type NoneOrMany(Type)                { ConDecl $2 ($1 : $3 : $4) }
                |   Type '`' Con '`' Type NoneOrMany(Type)          { ConDecl $3 ($1 : $5 : $6) }

ConstrOther     ::  { ConstrDecl }
                :   '|' Constr                                      { $2 }

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
                -- :   symid                                           { $1 }
                :   op                                              { $1 }
                |   '|'                                             { "|" }

OpCon           ::  { String }
                :   opcon                                           { $1 }

Oper            ::  { Expression }
                -- :   symid                                           { Op $1 }
                :   op                                              { Var $1 }
                |   '|'                                             { Var "|" }
                |   opcon                                           { Var $1 }

Exp             ::  { Expression }
                :   Var                                             { Var $1 }
                |   Con                                             { Var $1 }
                --  Note: Consider adding Constructor Expression for this ^^^
                -- |   '(' Oper ')'                                    { $2 }
                |   '(' op ')'                                      { Var $2 }
                |   '(' opcon ')'                                   { Var $2 }
                --  Note: Consider adding Constructor Expression for this ^^^
                |   symid                                           { Op $1 }
                -- NOTE: To resolve 2 R/R conflicts
                |   Lit                                             { $1 }
                |   lambda Params '->' Exp                          { foldr (\ arg body -> Lam arg body) $4 $2 }

                |   '(' Exp '`' Var '`' OneOrMany(Exp) ')'          { foldl App (Var $4) ($2 : $6) }
                |   '(' Exp '`' Con '`' OneOrMany(Exp) ')'          { foldl App (Var $4) ($2 : $6) }
                --  Note: Consider adding Constructor Expression for this ^^^
                |   '(' Exp Oper OneOrMany(Exp) ')'                 { foldl App $3 ($2 : $4) }
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
                |   letrec LowIdent Params '=' Exp in Exp           { Let $2 (Fix $ foldr (\ arg body -> Lam arg body) $5 ($2 : $3)) $7 }
                -- TODO: do the same for letrec
                |   '(' Exp CommaSeparated(Exp) ')'                 { Tuple $ $2 : $3 }
                |   '[' NoneOrManySeparated(Exp) ']'                 { foldr (\ item acc -> App (App (Var ":") item) acc ) (Var "[]") $2 }
                -- wiring the List type into the compiler

Binding         ::  { (String, Expression) }
                :   LowIdent '=' Exp                                { ($1, $3) }
                |   LowIdent Params '=' Exp                         { ($1, foldr (\ arg body -> Lam arg body) $4 $2) }
                |   '(' Op ')' Params '=' Exp                       { ($2, foldr (\ arg body -> Lam arg body) $6 $4) }
                |   Var Op Params '=' Exp                           { ($2, foldr Lam $5 ($1 : $3)) }
                |   Var '`' Var '`' Params '=' Exp                  { ($3, foldr Lam $7 ($1 : $5)) }
                
                |   rec LowIdent '=' Exp                            { ($2, Fix (Lam $2 $4)) }
                |   rec LowIdent Params '=' Exp                     { ($2, Fix $ foldr (\ arg body -> Lam arg body) $5 ($2 : $3)) }
                |   rec Var Op Params '=' Exp                       { ($3, Fix $ foldr Lam $6 ($3 : $2 : $4)) }
                |   rec '(' Op ')' Params '=' Exp                   { ($3, Fix $ foldr (\ arg body -> Lam arg body) $7 ($3 : $5)) }
                |   rec Var '`' Var '`' Params '=' Exp              { ($4, Fix $ foldr Lam $8 ($4 : $2 : $6)) }

GlobalBinding   ::  { (String, Expression) }
                :   LowIdent '=' Exp                                { ($1, $3) }
                |   LowIdent Params '=' Exp                         { ($1, foldr (\ arg body -> Lam arg body) $4 $2) }
                |   '(' Op ')' Params '=' Exp                       { ($2, foldr (\ arg body -> Lam arg body) $6 $4) }
                |   Var Op Params '=' Exp                           { ($2, foldr Lam $5 ($1 : $3)) }
                |   Var '`' Var '`' Params '=' Exp                  { ($3, foldr Lam $7 ($1 : $5)) }
                -- |   rec '(' Op ')' Params '=' Exp                   { ($3, Fix $ foldr (\ arg body -> Lam arg body) $7 ($3 : $5)) }
                -- |   rec LowIdent Params '=' Exp                     { ($2, Fix $ foldr (\ arg body -> Lam arg body) $5 ($2 : $3)) }
                -- |   rec Var Op Var '=' Exp                          { ($3, Fix $ Lam $3 (Lam $2 (Lam $4 $6))) }
                -- |   rec Var '`' Var '`' Var '=' Exp                 { ($4, Fix $ Lam $4 (Lam $2 (Lam $6 $8))) }

Decl            ::  { Declaration }
                :   GlobalBinding                                   { Binding (fst $1) (snd $1) }

Lit             ::  { Expression }
                :   Integer                                         { Lit $1 }
                |   Double                                          { Lit $1 }
                |   char                                            { Lit $ LitChar $1 }
                |   string                                          { foldr (\ item acc -> App (App (Var ":") (Lit $ LitChar item)) acc ) (Var "[]") $1 }
                --  LitString $1 }
                -- |   unit                                            { Lit LitUnit }

Integer         ::  { Lit }
                :   integer                                         { LitInt $1 }

Double          ::  { Lit }
                :   double                                          { LitDouble $1 }

Type            ::  { Type }
                :   LowIdent                                        { TyVar $1 }
                |   UpIdent                                         { TyCon $1 }
                |   TyArr                                           { $1 }
                |   TyTuple                                         { $1 }
                -- |   TyList                                          { $1 }
                |   '(' TyApp ')'                                   { $2 }
                |   '(' Type ')'                                    { $2 }

TyArr           ::  { Type }
                -- :   '(' TyArr ')' OneOrMany(TyArrRight)             { undefined }
                :   Type '->' Type                                  { TyArr $1 $3 }
                |   Type '->' TyArr                                 { TyArr $1 $3 }

-- TyArrRight      ::  { Type }
--                 :   Type '->'                                       { $1 }

TyTuple         ::  { Type }
                :   '(' Type CommaSeparated(Type) ')'               { TyTuple $ $2 : $3 }

-- TyList          ::  { Type }
--                 :   '[' Type ']'                                    { TyList $2 }

TyApp           ::  { Type }
                :   Type OneOrMany(Type)                            { foldl TyApp $1 $2 }

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


parse'expr :: String -> Either [Declaration] Expression
parse'expr s =
  evalP parserAct s
}