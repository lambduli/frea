module Interpreter.Evaluate where

import qualified Data.Map as Map
import Data.Either
import Data.List

import Compiler.Syntax.Expression
import Compiler.Syntax.Literal
import Interpreter.Value (EvaluationError(..), Env(..))
import qualified Interpreter.Value as Val
import Debug.Trace



force'val :: Either EvaluationError Val.Value -> Either EvaluationError Val.Value
force'val (Left err) = Left err
force'val (Right (Val.Thunk force'f)) = force'val $ force'f ()
force'val (Right val) = Right val


force :: Expression -> Env -> Either EvaluationError Val.Value
force expr env
  = case evaluate expr env of
      Right (Val.Thunk force'f) -> force'val $ force'f ()
      Right val -> Right val
      Left err -> Left err


evaluate :: Expression -> Env -> Either EvaluationError Val.Value
evaluate expr env@(Env bs) = case expr of
  Var name
    -- | Just (expr, env') <- lookup name bs -> trace ("0 eval var name = " ++ name ++ "  value = " ++ show expr) (evaluate expr env')
    | Just (expr, env') <- lookup name bs -> evaluate expr env'

    | otherwise -> Left $ UnboundVar name

  Op name
    | Just (expr, env') <- lookup name bs -> evaluate expr env' -- TODO: future -> no lookup for primitive operations
    | otherwise -> Right $ Val.Op name

  Lit lit ->
    Right $ Val.Lit lit

  Tuple exprs ->
    Right $ Val.Tuple $ map (\ expr -> Val.Thunk (\ _ -> evaluate expr env)) exprs -- used to be force

    -- let
    --   eiths = map (`evaluate` env) exprs
    --   may'err = find isLeft eiths
    -- in case may'err of
    --   Nothing ->
    --     let
    --       from'right (Right x) = x
    --       vals = map from'right eiths
    --     in Right $ Val.Tuple vals
    --   Just err -> err

  List exprs ->
    Right $ Val.List $ map (\ expr -> Val.Thunk (\ _ -> evaluate expr env)) exprs -- used to be force

    -- let
    --   eiths = map (`evaluate` env) exprs
    --   may'err = find isLeft eiths
    -- in case may'err of
    --   Nothing ->
    --     let
    --       from'right (Right x) = x
    --       vals = map from'right eiths
    --     in Right $ Val.List vals
    --   Just err -> err

  Let name val expr ->
    Right $ Val.Thunk (\ _ -> evaluate (App (Lam name expr) val) env) -- forced

  Lam par body ->
    -- Right $ trace ("_ evaluating lambda par = " ++ par) $ Val.Lam par body env
    Right $ Val.Lam par body env


  App (Lam par body) right ->
    -- Right $ trace ("3 eval app param = " ++ par)
    --   Val.Thunk (\ _ -> trace ("4 forcing app param = " ++ par) $ evaluate body (Val.Env ((par, (right, env)) : bs))) -- forced
    Right $ Val.Thunk (\ _ -> evaluate body (Val.Env ((par, (right, env)) : bs))) -- forced


  -- otazka je - mel bych evaluovat right pred tim nez provedu apply'operator?
  -- kdyz to neudelam, bude apply'operator muset evaluovat sama
  -- pokud by evaluovala -> mohla by dostat Thunk nebo hodnotu
  -- taky bych to ale mohl rovnou evalnout, protoze vsechny nativni operatory stejne budou tu operaci delat
  -- pokud v budoucnu nezavedu operator, kterej nebude evaluaci argumentu provadet, nebude problem
  -- takze tohle se asi nebude menit
  App (Op op) right ->
    case force right env of
      Right (Val.Thunk _) -> Left $ Unexpected "z nejakyho duvodu po forcu stejne mam thunk"
      Right r'val -> apply'operator op r'val env
      Left err -> Left err

  App left right ->
    -- Right $ trace "5 evaluate general APP" $ Val.Thunk (\ _ ->
    Right $ Val.Thunk (\ _ ->

      -- case trace ("6 force general APP  right = " ++ show right ) (force left env) of
      case force left env of
        Left err -> Left err
        Right (Val.Lam par body (Env bs')) ->
          -- trace "6.1 forcnul jsem left a dostal jsem lambdu" $
          force body $ Val.Env ((par, (right, env)) : bs')
        Right (Val.Thunk f'f) -> Left $ Unexpected "Po Forcu jsem dostal Thunk")

  If cond' then' else' ->
    Right $ Val.Thunk (\ _ ->
      case force cond' env of
        Left err -> Left err
        Right (Val.Lit (LitBool b)) ->
          if b then evaluate then' env else evaluate else' env)

  Fix expr ->
    -- Right $ trace "1 eval fix" Val.Thunk (\ _ ->
    --   trace "2 force fix thunk"
    Right $ Val.Thunk (\ _ ->
      evaluate (App expr $ Fix expr) env)


apply'operator :: String -> Val.Value -> Env -> Either EvaluationError Val.Value
apply'operator "#=" (Val.Tuple [val'l, val'r]) env
  | (Right (Val.Lit lit'l), Right (Val.Lit lit'r))
      <- (force'val $ Right val'l, force'val $ Right val'r)
        = Right $ Val.Lit (LitBool (lit'l == lit'r))

apply'operator "#<" (Val.Tuple [val'l, val'r]) env
  | (Right (Val.Lit lit'l), Right (Val.Lit lit'r))
      <- (force'val $ Right val'l, force'val $ Right val'r)
        = Right $ Val.Lit (LitBool (lit'l < lit'r))
-- apply'operator "#<" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt i'r)]) env
--   = Right $ Val.Lit (LitBool (i'l < i'r))

apply'operator "#>" (Val.Tuple [val'l, val'r]) env
  | (Right (Val.Lit lit'l), Right (Val.Lit lit'r))
      <- (force'val $ Right val'l, force'val $ Right val'r)
        = Right $ Val.Lit (LitBool (lit'l > lit'r))
-- apply'operator "#>" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt i'r)]) env
  -- = Right $ Val.Lit (LitBool (i'l > i'r))

apply'operator "#+" (Val.Tuple [val'l, val'r]) env
  | (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r)))
      <- (force'val $ Right val'l, force'val $ Right val'r)
        = Right $ Val.Lit (LitInt (i'l + i'r))

apply'operator "#+." (Val.Tuple [val'l, val'r]) env
  | (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r)))
      <- (force'val $ Right val'l, force'val $ Right val'r)
        = Right $ Val.Lit (LitDouble (d'l + d'r))
-- apply'operator "#+" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt i'r)]) env
  -- = Right $ Val.Lit (LitInt (i'l + i'r))

apply'operator "#*" (Val.Tuple [val'l, val'r]) env
  | (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r)))
      <- (force'val $ Right val'l, force'val $ Right val'r)
        = Right $ Val.Lit (LitInt (i'l * i'r))

apply'operator "#*." (Val.Tuple [val'l, val'r]) env
  | (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r)))
      <- (force'val $ Right val'l, force'val $ Right val'r)
        = Right $ Val.Lit (LitDouble (d'l * d'r))
-- apply'operator "#*" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt i'r)]) env
  -- = Right $ Val.Lit (LitInt (i'l * i'r))


apply'operator "#-" (Val.Tuple [val'l, val'r]) env
  | (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r)))
      <- (force'val $ Right val'l, force'val $ Right val'r)
        = Right $ Val.Lit (LitInt (i'l - i'r))

apply'operator "#-." (Val.Tuple [val'l, val'r]) env
  | (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r)))
      <- (force'val $ Right val'l, force'val $ Right val'r)
        = Right $ Val.Lit (LitDouble (d'l - d'r))
-- apply'operator "#-" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt i'r)]) env
  -- = Right $ Val.Lit (LitInt (i'l - i'r))


apply'operator "#div" (Val.Tuple [val'l, val'r]) env
  | (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt i'r)))
      <- (force'val $ Right val'l, force'val $ Right val'r)
        = Right $ Val.Lit (LitInt (i'l `div` i'r))
  | (Right (Val.Lit (LitInt i'l)), Right (Val.Lit (LitInt 0)))
      <- (force'val $ Right val'l, force'val $ Right val'r)
        = Left $ DivisionByZero i'l

apply'operator "#/" (Val.Tuple [val'l, val'r]) env
  | (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble d'r)))
      <- (force'val $ Right val'l, force'val $ Right val'r)
        = Right $ Val.Lit (LitDouble (d'l / d'r))
  | (Right (Val.Lit (LitDouble d'l)), Right (Val.Lit (LitDouble 0)))
      <- (force'val $ Right val'l, force'val $ Right val'r)
        = Left $ DivisionByZero 0


-- apply'operator "#/" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt 0)]) env
  -- = Left $ DivisionByZero i'l
-- apply'operator "#/" (Val.Tuple [Val.Lit (LitInt i'l), Val.Lit (LitInt i'r)]) env
  -- = Right $ Val.Lit (LitInt (i'l `div` i'r))

apply'operator "#++" (Val.Tuple [val'l, val'r]) env
  = Right $ Val.Thunk (\ _ ->
      case (force'val $ Right val'l, force'val $ Right val'r) of
        (Right (Val.List exprs'left), Right (Val.List exprs'right)) ->
          Right $ Val.List $ exprs'left ++ exprs'right
        (Right (Val.Lit (LitString str'left)), Right (Val.Lit (LitString str'right))) ->
          Right $ Val.Lit $ LitString $ str'left ++ str'right
        (Left err, _) -> Left err
        (_, Left err) -> Left err)
-- apply'operator "#++" (Val.Tuple [Val.List exprs'left, Val.List exprs'right]) env
  -- = Right $ Val.List $ exprs'left ++ exprs'right
-- 
-- apply'operator "#++" (Val.Tuple [Val.Lit (LitString str'left), Val.Lit (LitString str'right)]) env
  -- = Right $ Val.Lit $ LitString $ str'left ++ str'right

apply'operator "#:" (Val.Tuple [val'l, val'r]) env
  = Right $ Val.Thunk (\ _ ->
      case force'val $ Right val'r of
        Right (Val.Lit (LitString str))
          | Right (Val.Lit (LitChar ch)) <- force'val $ Right val'l ->
            Right $ Val.Lit $ LitString $ ch : str
          | Left err <- force'val $ Right val'l -> Left err
          | otherwise -> Left $ BadOperatorApplication "#:" val'l

        Right (Val.List exprs) -> Right $ Val.List $ val'l : exprs

        Left err -> Left err)

-- apply'operator "#:" (Val.Tuple [expr, Val.List exprs]) env
--   = Right $ Val.List $ expr : exprs 
-- apply'operator "#:" (Val.Tuple [Val.Lit (LitChar ch), Val.Lit (LitString str)]) env
--   = Right $ Val.Lit $ LitString $ ch : str

apply'operator "#head" (Val.List []) env
  = Right $ Val.Thunk (\ _ -> Left NilHeadException)

apply'operator "#head" (Val.Lit (LitString "")) env
  = Right $ Val.Thunk (\ _ -> Left EmptyStringException)

apply'operator "#head" (Val.List (e : es)) env
  = Right $ Val.Thunk (\ _ -> Right e)

apply'operator "#head" (Val.Lit (LitString (e : es))) env
  = Right $ Val.Thunk (\ _ -> Right $ Val.Lit $ LitChar e)

apply'operator "#tail" (Val.List []) env
  = Right $ Val.Thunk (\ _ -> Left NilTailException)

apply'operator "#tail" (Val.Lit (LitString "")) env
  = Right $ Val.Thunk (\ _ -> Left EmptyStringException)

apply'operator "#tail" (Val.List (e : es)) env
  = Right $ Val.Thunk (\ _ -> Right $ Val.List es)

apply'operator "#tail" (Val.Lit (LitString (e : es))) env
  = Right $ Val.Thunk (\ _ -> Right $ Val.Lit $ LitString es)

apply'operator "#nil?" (Val.List []) env
  = Right $ Val.Thunk (\ _ -> Right $ Val.Lit $ LitBool True)

apply'operator "#nil?" (Val.Lit (LitString "")) env
  = Right $ Val.Thunk (\ _ -> Right $ Val.Lit $ LitBool True)

apply'operator "#nil?" (Val.List (e : es)) env
  = Right $ Val.Thunk (\ _ -> Right $ Val.Lit $ LitBool False)

apply'operator "#nil?" (Val.Lit (LitString (e : es))) env
  = Right $ Val.Thunk (\ _ -> Right $ Val.Lit $ LitBool False)

apply'operator "#fst" (Val.Tuple [f, s]) env
  = Right $ Val.Thunk (\ _ -> Right f)

apply'operator "#snd" (Val.Tuple [f, s]) env
  = Right $ Val.Thunk (\ _ -> Right s)

apply'operator name expr env
  = Left $ BadOperatorApplication name expr
