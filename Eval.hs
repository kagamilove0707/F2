module Eval (eval) where

import DataType
import Control.Monad.Error
import Control.Monad.State

eval' :: AST -> StateT VEnv (Either String) Value
eval' (Let s e1 e2) = do
  v1 <- eval' e1
  modify ((s,v1):)
  v2 <- eval' e2
  modify tail
  return v2
eval' fn@(Fun s e) = do
  env <- get
  return $ VFun env fn
eval' fn@(App e1 e2) = do
  v1 <- eval' e1
  v2 <- eval' e2
  case v1 of
    VFun env (Fun s f) -> do
      envs <- get
      put $ (s, v2):env
      v3 <- eval' f
      put envs
      return v3
    VFFI f -> f v2
eval' (If c e1 e2) = do
  (VBool vc) <- eval' c
  if vc
    then eval' e1
    else eval' e2
eval' (Tuple (e1, e2)) = do
  v1 <- eval' e1
  v2 <- eval' e2
  return (VTuple (v1,v2))
eval' (Var s) = do
  env <- get
  case lookup s env of
    Just v -> return v
eval' (IntLit x) = return $ VInt x
eval' (BoolLit x) = return $ VBool x

eval :: VEnv -> AST -> Either String Value
eval env ast = evalStateT (eval' ast) env
