module F2 (
  exec, execPrelude, preludeEnv,
  module DataType) where

import DataType
import Parser
import TypeInterface
import Eval

import Control.Monad.Error

defaultEnv :: Env
defaultEnv = []

preludeEnv :: Env
preludeEnv = [
  ("id", ((TFun (TVar "'a") (TVar "'a")),
         (VFFI (\x -> return $ x)))),
  ("+", ((TFun TInt (TFun TInt TInt)),
          (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> return $ VInt (x + y))))))]

exec :: Int -> Env -> String -> Either String (Type, Value, Int)
exec n env src = do
  ast <- parse src
  ((_, t, _), m) <- tinf (toTyEnv env) ast n
  v <- eval (toVEnv env) ast
  return (t, v, m)

execPrelude = exec 0 preludeEnv

toTyEnv = map (\(s, (t, _))-> (s, t))
toVEnv = map (\(s, (_, v))-> (s, v))
