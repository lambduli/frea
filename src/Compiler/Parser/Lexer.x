{
module Compiler.Parser.Lexer (lexer, readToken) where

import Control.Monad.State

import Compiler.Parser.Utils
import Compiler.Parser.Token
}

$digit      = [0-9]
$octit      = [0-7]
$hexit      = [a-f A-F $digit]

@decimal      = $digit+
@octal        = $octit+
@hexadecimal  = $hexit+
@exponent     = [eE] [\-\+] @decimal

$lower  = [a-z]
$upper  = [A-Z]

$symbol  = [ \! \$ \# \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~ \; ]

$identchar    = [$lower $upper $digit $symbol]

@variableident      = $lower $identchar*

@constrident        = $upper $identchar*

@operator     = $symbol [$lower $upper $symbol \:]*

@opconstr     = [\:] [$symbol $lower $upper \:]*

$space  = [\ \t\f\v]


--
-- lexical grammar
--

token :-

-- reserved keywords
<0>         assume          { plainTok TokAssume }
<0>         rec             { plainTok TokRec }

<0>         match           { plainTok TokMatch }
<0>         with            { plainTok TokWith }
<0>         data            { plainTok TokData }
<0>         if              { plainTok TokIf }
<0>         then            { plainTok TokThen }
<0>         else            { plainTok TokElse }
<0>         "let rec"       { plainTok $ TokLetrec }
<0>         let             { plainTok TokLet }
<0>         in              { plainTok TokIn }
<0>         type            { plainTok TokType }
<0>         \_              { plainTok TokHole }
<0>         the             { plainTok TokThe }
<0>         claim           { plainTok TokClaim }
<0>         define          { plainTok TokDefine }
<0>         lambda          { plainTok TokLambda }
<0>         "\"             { plainTok TokLambda }
<0>         fix             { plainTok $ TokFix }


-- special symbols
-- [ \( \) \, \[ \] \` \' \{ \} ]
<0>         "()"            { plainTok TokUnit }
<0>         "("             { plainTok TokLeftParen }
<0>         ")"             { plainTok TokRightParen }
<0>         "[" $space* "]" { parametrizedTok TokVarUpper (const "[]") }
<0>         "["             { plainTok TokLeftBracket }
<0>         "]"             { plainTok TokRightBracket }
<0>         ","             { plainTok TokComma }
<0>         "`"             { plainTok TokBackTick }

<0>         "#&&"           { plainTok $ TokNativeSym "#&&" }
<0>         "#||"           { plainTok $ TokNativeSym "#||" }

<0>         "#="            { plainTok $ TokNativeSym "#=" }
<0>         "#<"            { plainTok $ TokNativeSym "#<" }
<0>         "#>"            { plainTok $ TokNativeSym "#>" }
<0>         "#+"            { plainTok $ TokNativeSym "#+" }
<0>         "#+."           { plainTok $ TokNativeSym "#+." }
<0>         "#-"            { plainTok $ TokNativeSym "#-" }
<0>         "#-."           { plainTok $ TokNativeSym "#-." }
<0>         "#*"            { plainTok $ TokNativeSym "#*" }
<0>         "#*."           { plainTok $ TokNativeSym "#*." }
<0>         "#div"          { plainTok $ TokNativeSym "#div" }
<0>         "#/"            { plainTok $ TokNativeSym "#/" }
<0>         "#fst"          { plainTok $ TokNativeSym "#fst" }
<0>         "#snd"          { plainTok $ TokNativeSym "#snd" }
<0>         "#show"         { plainTok $ TokNativeSym "#show" }

-- <0>         "#showint"      { plainTok $ TokNativeSym "#showint" }
-- <0>         "#showbool"     { plainTok $ TokNativeSym "#showbool" }
-- <0>         "#showdouble"   { plainTok $ TokNativeSym "#showdouble" }
-- <0>         "#showchar"     { plainTok $ TokNativeSym "#showchar" }
-- <0>         "#showstring"   { plainTok $ TokNativeSym "#showstring" }
-- <0>         "#showtuple"    { plainTok $ TokNativeSym "#showtuple" }
-- <0>         "#showlist"     { plainTok $ TokNativeSym "#showlist" }
-- <0>         "#showunit"     { plainTok $ TokNativeSym "#showunit" }

-- variables and constructors - qualified and un-qualified
<0>         @variableident  { parametrizedTok TokVarLower id }
<0>         @constrident    { parametrizedTok TokVarUpper id }
<0>         @operator       { parametrizedTok TokOperator id }
<0>         @opconstr       { parametrizedTok TokOpConstr id }

<0>         \n              ;
<0>         $space+         ;


-- literals

<0>         @decimal
          | \-@decimal
          | 0[oO] @octal
          | \-0[oO] @octal
          | 0[xX] @hexadecimal
          | \-0[xX] @hexadecimal  { parametrizedTok TokInt read }

<0>         @decimal \. @decimal @exponent?
          | \-@decimal \. @decimal @exponent?
          | @decimal @exponent
          | \-@decimal @exponent  { parametrizedTok TokDouble read }


<0>         \'              { beginChar }
<charSC>    .{1, 2}\'       { readChar }

<0>         \"              { beginString }
<stringSC>  \"              { endString }
<stringSC>  \\['\"\\nrtbfv] { escapeString }
<stringSC>  \\.             { swallowBackslash }
<stringSC>  .               { appendString }


<0>         \"\"\"          { beginMultilineString }
<mlStrSC>   \"{3,}          { endMultilineString }
<mlStrSC>   \\['\"\\nrtbfv] { escapeString }
<mlStrSC>   \\.             { swallowBackslash }
<mlStrSC>   .               { appendString }
<mlStrSC>   \n              { carryReturnString }


<0>         \-\-.*\n          ;

{

type NumberOfCharsMatched = Int

type MatchedSequence = String

type LexAction = NumberOfCharsMatched -> MatchedSequence -> P (Maybe Token)


plainTok :: Token -> LexAction
plainTok token _ _ = do
  -- pos <- getPosition len
  -- let token = tc pos
  return $ Just token


parametrizedTok :: (a -> Token) -> (String -> a) -> LexAction
parametrizedTok tc read' _ matched = do
  -- pos <- getPosition len
  let token = tc (read' matched)
  return $ Just token


beginChar :: LexAction
beginChar _ _ = do
  s <- get
  put s{ lexSC = charSC } -- TODO: maybe also push previous SC to the previousSC
  return Nothing


readChar :: LexAction
readChar 2 (c : '\'' : []) = do
  s <- get
  pos <- getPosition 2
  put s{ lexSC = 0 } -- TODO: pick from previousSC?
  let token = TokChar c -- pos
  return $ Just token


readChar 3 ('\\' : seq : '\'' : []) = do
  let unesc =
        case seq of
          '\'' -> '\''
          {- '"' -> '"' -- is this needed? -}
          '\\' -> '\\'
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'
          'b' -> '\b'
          'f' -> '\f'
          'v' -> '\v'
          _   -> seq
  s <- get
  pos <- getPosition 3
  put s{ lexSC = 0 } -- TODO: maybe pick from previousSC?
  let token = TokChar unesc -- pos
  return $ Just token


beginString :: LexAction
beginString len _ = do
  s <- get
  pos <- getPosition len
  put  s{ lexSC = stringSC, pending'position = pos } -- , previousSC = (lexSC s) : (previousSC s) -- TODO:
  -- test and uncomment eventually - should be fine
  return Nothing


beginMultilineString :: LexAction
beginMultilineString len _ = do
  s <- get
  pos <- getPosition len
  put  s{ lexSC = mlStrSC
        -- , previousSC = (lexSC s) : (previousSC s)
        , pending'position = pos }
  return Nothing


appendString :: LexAction
appendString _ (c : _) = do
  s <- get
  put s{ stringBuf = c : (stringBuf s) }
  return Nothing


escapeString :: LexAction
escapeString 2 ('\\' : c : []) = do
  let unesc =
        case c of
          {- '\'' -> '\'' -- is this needed? -}
          '"' -> '"'
          '\\' -> '\\'
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'
          'b' -> '\b'
          'f' -> '\f'
          'v' -> '\v'
          _   -> c
  s <- get
  put s{ stringBuf = unesc : (stringBuf s) }
  return Nothing


swallowBackslash :: LexAction
swallowBackslash 2 ('\\' : c : []) = do
  s <- get
  put s{ stringBuf = c : (stringBuf s) }
  return Nothing


carryReturnString :: LexAction
carryReturnString _ _ = do
  s <- get
  put s{ stringBuf = '\n' : (stringBuf s) }
  return Nothing


endString :: LexAction
endString _ _ = do
  s <- get
  -- (lexSC', prevSCs') <- getPreviousSC
  let buf = stringBuf s
  put  s{ lexSC = 0
--        , previousSC = prevSCs' -- TODO: test and uncomment eventually - should be fine
        , stringBuf = "" }
  let token = TokString (reverse buf) -- (pending'position s)
  return $ Just token


endMultilineString :: LexAction
endMultilineString count _ = do
  s <- get
  -- (lexSC', prevSCs') <- getPreviousSC
  let buf = stringBuf s
  put  s{ lexSC = 0
        -- , previousSC = prevSCs'
        , stringBuf = "" }
  let token = TokString (reverse $ replicate (count - 3) '"' ++ buf) -- (pending'position s)
  return $ Just token


-- TODO: maybe also define empty Constructor for TokPosition
-- use it when constructing and here compute position and update it and return complete and correct Token
-- TODO2: I may actually not need to store position in the Token at all
-- thanks to monadic parsing I have the state at every moment of parsing --> just need to find out
-- how to use the state for better parse-error messages
readToken :: P Token
readToken = do
  s <- get
  case pending'tokens s of
    (tok : toks) -> do
      put s{ pending'tokens = toks }
      return tok

    [] ->
      case alexScan (input s) (lexSC s) of
        AlexEOF -> do
          return TokEOF

        AlexError inp' -> error $ "Lexical error on line " ++ (show $ ai'line'number inp')
        
        AlexSkip inp' _ -> do
          put s{ input = inp' }
          readToken
        
        AlexToken inp' n act -> do
          -- let ll = layout'stack s
          let (AlexInput{ ai'rest = buf }) = input s -- TODO: rename airest
          put s{ input = inp' }
          res <- act n (take n buf)
          case res of
            Nothing -> readToken
            Just t -> return t


lexer :: (Token -> P a) -> P a
lexer cont = do
  tok <- readToken
  cont tok
}
