module Main (main) where

import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Foldable (traverse_)

import Lib
import Optics

newtype App a = App {unApp :: ReaderT Env (ExceptT RuntimeError IO) a}
    deriving stock
        ( Generic
        )
    deriving newtype
        ( Applicative
        , Functor
        , Monad
        , MonadError RuntimeError
        , MonadIO
        , MonadReader Env
        )

runApp :: Env -> App a -> IO (Either RuntimeError a)
runApp env = runExceptT . flip runReaderT env . (^. #unApp)

programs :: [Program]
programs =
    [ Let
        { lhs = "x"
        , rhs = Const 1
        , body =
            Let
                { lhs = "f"
                , rhs = Proc{param = "y", body = Add (Var "x") (Var "y")}
                , body =
                    Let
                        { lhs = "x"
                        , rhs = Const 2
                        , body =
                            Let
                                { lhs = "g"
                                , rhs = Proc{param = "y", body = Add (Var "x") (Var "y")}
                                , body = Add (Call (Var "f") (Const 1)) (Call (Var "g") (Const 1))
                                }
                        }
                }
        }
    , LetRec
        { nProc =
            NamedProc
                { name = "double"
                , param = "x"
                , body =
                    If
                        { cond = Eq (Var "x") (Const 0)
                        , thenClause = Const 0
                        , elseClause = Add (Call (Var "double") (Sub (Var "x") (Const 1))) (Const 2)
                        }
                }
        , body = Call (Var "double") (Const 6)
        }
    , LetMRec
        { nProc1 =
            NamedProc
                { name = "even"
                , param = "x"
                , body =
                    If
                        { cond = Eq (Var "x") (Const 0)
                        , thenClause = ETrue
                        , elseClause = Call (Var "odd") (Sub (Var "x") (Const 1))
                        }
                }
        , nProc2 =
            NamedProc
                { name = "odd"
                , param = "x"
                , body =
                    If
                        { cond = Eq (Var "x") (Const 0)
                        , thenClause = EFalse
                        , elseClause = Call (Var "even") (Sub (Var "x") (Const 1))
                        }
                }
        , body = Call (Var "odd") (Const 13)
        }
    , LetRec
        { nProc =
            NamedProc
                { name = "factorial"
                , param = "x"
                , body =
                    If
                        { cond = Eq (Var "x") (Const 0)
                        , thenClause = Const 1
                        , elseClause = Mul (Call (Var "factorial") (Sub (Var "x") (Const 1))) (Var "x")
                        }
                }
        , body =
            LetRec
                { nProc =
                    NamedProc
                        { name = "loop"
                        , param = "n"
                        , body =
                            If
                                { cond = Eq (Var "n") (Const 0)
                                , thenClause = Unit
                                , elseClause =
                                    Seq
                                        (Print (Call (Var "factorial") (Var "n")))
                                        (Call (Var "loop") (Sub (Var "n") (Const 1)))
                                }
                        }
                , body = Call (Var "loop") (Const 10)
                }
        }
    , LetRec
        { nProc =
            NamedProc
                { name = "range"
                , param = "n"
                , body =
                    If
                        { cond = Eq (Var "n") (Const 1)
                        , thenClause = Cons (Const 1) Nil
                        , elseClause = Cons (Var "n") (Call (Var "range") (Sub (Var "n") (Const 1)))
                        }
                }
        , body = Call (Var "range") (Const 10)
        }
    , LetRec
        { nProc =
            NamedProc
                { name = "reverse"
                , param = "l"
                , body =
                    If
                        { cond = IsNil (Var "l")
                        , thenClause = Nil
                        , elseClause =
                            Append
                                (Call (Var "reverse") (Tail (Var "l")))
                                (Cons (Head (Var "l")) Nil)
                        }
                }
        , body =
            Call (Var "reverse") (Cons (Const 1) (Cons (Const 2) (Cons (Const 3) Nil)))
        }
    , Let
        { lhs = "fix"
        , rhs =
            Proc
                { param = "f"
                , body =
                    Call
                        ( Proc
                            { param = "x"
                            , body =
                                Call
                                    (Var "f")
                                    ( Proc
                                        { param = "y"
                                        , body = Call (Call (Var "x") (Var "x")) (Var "y")
                                        }
                                    )
                            }
                        )
                        ( Proc
                            { param = "x"
                            , body =
                                Call
                                    (Var "f")
                                    ( Proc
                                        { param = "y"
                                        , body = Call (Call (Var "x") (Var "x")) (Var "y")
                                        }
                                    )
                            }
                        )
                }
        , body =
            Let
                { lhs = "f"
                , rhs =
                    Call
                        (Var "fix")
                        ( Proc
                            { param = "range"
                            , body =
                                Proc
                                    { param = "n"
                                    , body =
                                        If
                                            { cond = Eq (Var "n") (Const 1)
                                            , thenClause = Cons (Const 1) Nil
                                            , elseClause = Cons (Var "n") (Call (Var "range") (Sub (Var "n") (Const 1)))
                                            }
                                    }
                            }
                        )
                , body = Call (Var "f") (Const 10)
                }
        }
    ]

main :: IO ()
main = do
    traverse_ go (zip [1 ..] programs)
  where
    go :: (Int, Program) -> IO ()
    go (num, program) = do
        putStrLn $ "[CASE " <> show num <> "]"
        result <- runApp emptyEnv $ runMl program

        putStrLn case result of
            Right v -> "[Return] : " <> show v
            Left e -> "[Error] : " <> show e
        putStrLn ""
