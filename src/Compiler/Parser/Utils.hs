module Compiler.Parser.Utils where

-- ACKNOWLEDGMENT
-- https://www.jyotirmoy.net/posts/2015-08-17-alex-happy-startcodes.html

import Control.Monad.State
import Control.Monad
import Data.Word
import Codec.Binary.UTF8.String (encode)

import Compiler.Parser.Token (Token (..), TokPosition(..))


-- Parser monad
type P a = State ParseState a


data AlexInput = AlexInput
  { ai'prev :: Char
  , ai'bytes :: [Word8]
  , ai'rest :: String
  , ai'line'number :: Int
  , ai'column'number :: Int }
  deriving Show


data ParseState = ParseState
  { input :: AlexInput
  , lexSC :: Int                      -- lexer start code
  , stringBuf :: String               -- temporary storage for strings
  , pending'tokens :: [Token]         -- right now used when Parser consumes the lookeahead and decided to put it back
  , pending'position :: TokPosition } -- needed when parsing strings, chars, multi-line strings
  deriving Show


initialState :: String -> ParseState
initialState s = ParseState
  { input = AlexInput
    { ai'prev = '\n'
    , ai'bytes = []
    , ai'rest = s
    , ai'line'number = 1
    , ai'column'number = 1 }
  , lexSC = 0
  , stringBuf = ""
  , pending'tokens = []
  , pending'position = TokPosition { line = 1, column = 1 }}


-- The functions that must be provided to Alex's basic interface

-- The input: last character, unused bytes, remaining string
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai =
  case (ai'bytes ai) of
    (b : bs) ->
      Just (b, ai{ ai'bytes = bs })

    [] ->
      case (ai'rest ai) of
        [] -> Nothing

        (char : chars) ->
          let
            n = (ai'line'number ai)
            n' = if char == '\n' then n + 1 else n
            c = (ai'column'number ai)
            c' = if char == '\n' then 1 else c + 1
            (b : bs) = encode [char]
          in
            Just (b, AlexInput  { ai'prev = char
                                , ai'bytes = bs
                                , ai'rest = chars
                                , ai'line'number = n'
                                , ai'column'number = c' })


-- alexInputPrevChar :: AlexInput -> Char
-- alexInputPrevChar (AlexInput { ai'prev = c }) = c


getLineNo :: P Int
getLineNo = do
  s <- get
  return . ai'line'number . input $ s


getColNo :: P Int
getColNo = do
  s <- get
  return . ai'column'number . input $ s


-- TODO: fix pls
-- what if current token begins on different line, like () or []
-- I know parsing () and [] like single token is probably not gonna last
-- but consider changing the logic anyways
-- I am thinking something like
-- when reading next char from current byte, lets store current position
-- and function readToken will pick that position and give it as a last argument to the Token
-- that doesn't really seem elegant
getPosition :: Int -> P TokPosition
getPosition tok'len = do
  ParseState { input = AlexInput { ai'line'number = ln, ai'column'number = cn } } <- get
  return $ TokPosition { line = ln, column = cn - tok'len }


evalP :: P a -> String -> a
evalP m s = evalState m (initialState s)