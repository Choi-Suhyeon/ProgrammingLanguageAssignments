{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lib where

import Control.Monad ((>=>))
import Control.Monad.Except (MonadError (..), liftEither)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..))
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Generics.Labels ()

import Optics
import Util

data Exp
    = Unit
    | ETrue
    | EFalse
    | Const Int
    | Var Variable
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp
    | Eq Exp Exp
    | Lt Exp Exp
    | Not Exp
    | Nil
    | Cons Exp Exp
    | Append Exp Exp
    | Head Exp
    | Tail Exp
    | IsNil Exp
    | If {cond :: Exp, thenClause :: Exp, elseClause :: Exp}
    | Proc {param :: Variable, body :: Exp}
    | Let {lhs :: Variable, rhs :: Exp, body :: Exp}
    | LetRec {nProc :: NamedProc, body :: Exp}
    | LetMRec {nProc1 :: NamedProc, nProc2 :: NamedProc, body :: Exp}
    | Call Exp Exp
    | Print Exp
    | Seq Exp Exp
    deriving (Eq, Generic)

data NamedProc = NamedProc {name :: Variable, param :: Variable, body :: Exp}
    deriving (Eq, Generic)

type Program = Exp
type Variable = String

data Value
    = VUnit
    | VInt Int
    | VBool Bool
    | VList [Value]
    | VProc {param :: Variable, body :: Exp, env :: Env}
    | VRecProc {name :: Variable, param :: Variable, body :: Exp, env :: Env}
    | VMRecProc
        { name1 :: Variable
        , param1 :: Variable
        , body1 :: Exp
        , name2 :: Variable
        , param2 :: Variable
        , body2 :: Exp
        , env :: Env
        }
    deriving (Generic)

instance Show Value where
    show (VInt n) = "Int " <> show n
    show (VList l) = "List " <> show l
    show (VBool b) = "Bool " <> map toLower (show b)
    show VUnit = "Unit unit"
    show _ = "Proc <proc>"

data RuntimeError
    = VarNotInScope Variable
    | TypeErrorBinOp
    | TypeErrorUnaryOp
    | ExpectedEquatable
    | ExpectedProcedure
    | ExpectedInt
    | ExpectedBool
    | ExpectedList
    | DivisionByZero
    | EmptyList
    deriving (Eq, Generic, Show)

data Equatable = EInt Int | EBool Bool

newtype Env = Env {unEnv :: Variable -> Either RuntimeError Value}
    deriving (Generic)

emptyEnv :: Env
emptyEnv = Env \v -> Left $ VarNotInScope v

extendEnv :: Variable -> Value -> Env -> Env
extendEnv var val env = Env \var' -> bool (env.unEnv var') (Right val) (var' == var)

runMl :: (MonadReader Env m, MonadError RuntimeError m, MonadIO m) => Program -> m Value
runMl Unit = pure VUnit
runMl ETrue = pure $ VBool True
runMl EFalse = pure $ VBool False
runMl (Const n) = pure $ VInt n
runMl (Var x) = liftEither . ($ x) . (^. #unEnv) =<< ask
runMl (Add e1 e2) = runBinOp ensureVInt ((Right . VInt) .: (+)) e1 e2
runMl (Sub e1 e2) = runBinOp ensureVInt ((Right . VInt) .: (-)) e1 e2
runMl (Mul e1 e2) = runBinOp ensureVInt ((Right . VInt) .: (*)) e1 e2
runMl (Div e1 e2) = runBinOp ensureVInt ((fmap VInt .) . safeDiv) e1 e2
  where
    safeDiv :: Int -> Int -> Either RuntimeError Int
    safeDiv x y
        | y /= 0 = Right $ x `div` y
        | otherwise = Left DivisionByZero
runMl (Eq e1 e2) = runBinOp ensureEq safeEq e1 e2
  where
    safeEq :: Equatable -> Equatable -> Either RuntimeError Value
    safeEq (EInt n1) (EInt n2) = n1 == n2 & VBool & Right
    safeEq (EBool b1) (EBool b2) = b1 == b2 & VBool & Right
    safeEq _ _ = Left TypeErrorBinOp
runMl (Lt e1 e2) = runBinOp ensureVInt ((Right . VBool) .: (<)) e1 e2
runMl (Not e) =
    runMl e >>= \case
        VBool True -> pure $ VBool False
        VBool False -> pure $ VBool True
        _ -> throwError TypeErrorUnaryOp
runMl Nil = pure $ VList []
runMl (Cons e1 e2) = runBinOp Right cons e1 e2
  where
    cons :: Value -> Value -> Either RuntimeError Value
    cons v (VList vs) = v : vs & VList & Right
    cons _ _ = Left TypeErrorBinOp
runMl (Append e1 e2) = runBinOp ensureVList ((Right . VList) .: (<>)) e1 e2
runMl (Head e) =
    runMl e >>= \case
        VList (x : _) -> pure $ x
        VList [] -> throwError EmptyList
        _ -> throwError TypeErrorUnaryOp
runMl (Tail e) =
    runMl e >>= \case
        VList (_ : xs) -> pure $ VList xs
        VList [] -> throwError EmptyList
        _ -> throwError TypeErrorUnaryOp
runMl (IsNil e) =
    runMl e >>= \case
        VList [] -> pure $ VBool True
        VList _ -> pure $ VBool False
        _ -> throwError TypeErrorUnaryOp
runMl If{cond, thenClause, elseClause} = runMl cond >>= liftEither . ensureVBool >>= runMl . bool elseClause thenClause
runMl Proc{param, body} = ask >>= \env -> pure $ VProc{param, body, env}
runMl Let{lhs, rhs, body} = (`local` runMl body) . extendEnv lhs =<< runMl rhs
runMl LetRec{nProc = p, body} = ask >>= \env -> local (extendEnv p.name VRecProc{name = p.name, param = p.param, body = p.body, env}) (runMl body)
runMl LetMRec{nProc1 = p1, nProc2 = p2, body} =
    ask >>= \env ->
        (`local` runMl body)
            ( extendEnv p1.name VMRecProc{name1 = p1.name, param1 = p1.param, body1 = p1.body, name2 = p2.name, param2 = p2.param, body2 = p2.body, env}
                . extendEnv p2.name VMRecProc{name1 = p2.name, param1 = p2.param, body1 = p2.body, name2 = p1.name, param2 = p1.param, body2 = p1.body, env}
            )
runMl (Call e1 e2) =
    runMl e2 >>= \a ->
        runMl e1 >>= \case
            VProc{param, body, env} -> (`local` runMl body) \_ -> extendEnv param a env
            recProc@VRecProc{name, param, body, env} -> (`local` runMl body)
                \_ ->
                    env
                        & extendEnv param a
                        & extendEnv name recProc
            recMProc@VMRecProc{name1, param1, body1, name2, param2, body2, env} -> (`local` runMl body1)
                \_ ->
                    env
                        & extendEnv param1 a
                        & extendEnv name1 recMProc
                        & extendEnv name2 VMRecProc{name1 = name2, param1 = param2, body1 = body2, name2 = name1, param2 = param1, body2 = body1, env}
            _ -> throwError ExpectedProcedure
runMl (Print e) = runMl e >>= liftIO . putStrLn . toPrintStrFromValue >> pure VUnit
  where
    toPrintStrFromValue :: Value -> String
    toPrintStrFromValue (VInt n) = show n
    toPrintStrFromValue (VBool b) = show b
    toPrintStrFromValue (VList l) = show l
    toPrintStrFromValue VUnit = "Unit"
    toPrintStrFromValue _ = "<proc>"
runMl (Seq e1 e2) = runMl e1 >> runMl e2

ensureEq :: Value -> Either RuntimeError Equatable
ensureEq (VInt n) = Right $ EInt n
ensureEq (VBool b) = Right $ EBool b
ensureEq _ = Left ExpectedEquatable

ensureVBool :: Value -> Either RuntimeError Bool
ensureVBool (VBool b) = Right b
ensureVBool _ = Left ExpectedBool

ensureVInt :: Value -> Either RuntimeError Int
ensureVInt (VInt n) = Right n
ensureVInt _ = Left ExpectedInt

ensureVList :: Value -> Either RuntimeError [Value]
ensureVList (VList xs) = Right xs
ensureVList _ = Left ExpectedList

runBinOp ::
    (MonadReader Env m, MonadError RuntimeError m, MonadIO m) =>
    (Value -> Either RuntimeError a) ->
    (a -> a -> Either RuntimeError Value) ->
    Exp ->
    Exp ->
    m Value
runBinOp p f e1 e2 = liftEither =<< f <$> run e1 <*> run e2
  where
    run = runMl >=> liftEither . p
