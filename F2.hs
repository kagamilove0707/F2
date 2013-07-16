module F2 (
  exec, execPrelude, preludeEnv, version,
  module DataType) where

import DataType
import Parser
import TypeInterface
import Eval

import Control.Monad.Error

version = "0.1.0 (2013/07/16)"

defaultEnv :: Env
defaultEnv = []

preludeEnv :: Env
preludeEnv = [
  ("id", ((TFun (TVar "'a") (TVar "'a")),
         (VFFI (\x -> return $ x)))),
  ("+", ((TFun TInt (TFun TInt TInt)),
          (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> return $ VInt (x + y)))))),
  ("=", ((TFun TInt (TFun TInt TBool)),
          (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> return $ VBool (x == y))))))]

exec :: Env -> String -> Either String (Type, Value)
exec env src = do
  ast <- parse src
  (_, t, _) <- tinf (toTyEnv env) ast
  v <- eval (toVEnv env) ast
  return (t, v)

execPrelude = exec preludeEnv

toTyEnv = map (\(s, (t, _))-> (s, t))
toVEnv = map (\(s, (_, v))-> (s, v))
