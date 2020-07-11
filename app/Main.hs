{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Reader (MonadReader (ask), ReaderT, local, runReaderT)
import Control.Monad.State.Strict (MonadState (get), StateT, modify, runStateT)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec
import GHC.Stack (HasCallStack)

data SExpr
  = Symbol String
  | IntLit Int
  | List [SExpr]
  deriving stock (Show, Eq, Ord)

data Value
  = SymbolVal String
  | IntVal Int
  | FuncVal VFun
  | Cons {_car :: Value, _cdr :: Value}
  | Nil
  deriving stock (Show, Eq, Ord)

data VFun = VFun ([Value] -> EvalM Value)

instance Show VFun where
  show (VFun _) = "<function>"

instance Eq VFun where
  _ == _ = True

instance Ord VFun where
  _ `compare` _ = EQ

type GlobalEnv = Map String Value

type LocalEnv = Map String Value

type EvalM a = ReaderT LocalEnv (StateT GlobalEnv IO) a

eval :: HasCallStack => SExpr -> EvalM Value
eval (Symbol s) = do
  genv <- get
  lenv <- ask
  case Map.lookup s lenv <|> Map.lookup s genv of
    Just val -> pure val
    Nothing -> error (s <> " is not defined")
eval (IntLit i) = pure $ IntVal i
eval (List [Symbol "define", Symbol name, value]) = do
  value' <- eval value
  modify (Map.insert name value')
  pure Nil
eval (List [Symbol "lambda", List ps, body]) = do
  let params =
        map
          ( \case
              Symbol x -> x
              x -> error (show x <> " must be a symbol")
          )
          ps
  lenv <- ask
  pure $
    FuncVal $
      VFun $ \as -> do
        let lenv' = Map.fromList $ zip params as
        local ((lenv' <> lenv) <>) $ eval body
eval (List (f : xs)) = do
  FuncVal (VFun f') <- eval f
  xs' <- traverse eval xs
  f' xs'

globalEnv :: GlobalEnv
globalEnv =
  Map.fromList
    [ ( "atom",
        FuncVal $
          VFun $ \[x] ->
            case x of
              SymbolVal {} -> pure (SymbolVal "t")
              IntVal {} -> pure (SymbolVal "t")
              Nil -> pure (SymbolVal "t")
              _ -> pure Nil
      )
    ]

sample :: [SExpr]
sample =
  [ List [Symbol "atom", IntLit 42],
    List [Symbol "define", Symbol "answer", IntLit 42],
    Symbol "answer"
  ]

k :: SExpr
k = List [Symbol "lambda", List [Symbol "x", Symbol "y"], Symbol "x"]

-- 遅延評価だと実行できるが正格評価では実行できない
-- このインタプリタは正格評価
testLazy :: SExpr
testLazy = List [k, k, List [Symbol "atom", k, k]]

main :: IO ()
main = do
  val <- runStateT (runReaderT (traverse eval sample) mempty) globalEnv
  print val
