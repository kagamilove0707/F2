module Language.F2.Eval (eval, getInt, getBool, getTuple, applyValue, getValue) where

import Language.F2.DataType
import Control.Monad.Error
import Control.Monad.State

getInt :: Value -> Either String Integer
getInt v = do
  v' <- getValue v
  case v' of
    VInt x -> return x
    _ -> throwError "exec error : type miss..."

getBool :: Value -> Either String Bool
getBool v = do
  v' <- getValue v
  case v' of
    VBool x -> return x
    _ -> throwError "exec error : type miss..."

getTuple :: Value -> Either String (Value, Value)
getTuple v = do
  v' <- getValue v
  case v' of
    VTuple x -> return x
    _ -> throwError "exec error : type miss..."

applyValue :: Value -> Value -> Either String Value
applyValue v x = do
  v' <- getValue v
  case v' of
    VFun env (Fun s ast) -> eval ((s, x):env) ast
    VFFI f -> f v

getValue :: Value -> Either String Value
getValue (VLazy env ast) = eval env ast
getValue x = return x

eval' :: AST -> StateT VEnv (Either String) Value
eval' (Let s e1 e2) = do
  v1 <- eval' e1
  modify ((s,v1):)
  v2 <- eval' e2
  modify tail
  return v2
eval' (LetRec s e1 e2) = do
  v1 <- eval' e1
  case v1 of
    VFun env e -> do
      let fn = VFun ((s, fn):env) e
      modify ((s,fn):)
    _ -> modify ((s, v1):)
  v2 <- eval' e2
  modify tail
  return v2
eval' fn@(Fun s e) = do
  env <- get
  return $ VFun env fn
eval' fn@(App e1 e2) = do
  v1 <- eval' e1 >>= lift . getValue
  v2 <- eval' e2
  case v1 of
    VFun env (Fun s f) -> do
      envs <- get
      put $ (s, v2):env
      v3 <- eval' f
      put envs
      return v3
    VFFI f -> lift $ f v2
eval' (If c e1 e2) = do
  vc <- eval' c >>= lift . getBool
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
eval' (Sig ast _) = eval' ast
eval' (Lazy ast) = do
  env <- get
  return $ VLazy env ast

eval :: VEnv -> AST -> Either String Value
eval env ast = evalStateT (eval' ast) env
