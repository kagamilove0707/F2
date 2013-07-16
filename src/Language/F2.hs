module Language.F2 (
  exec, execPrelude, preludeEnv, version,
  module Language.F2.DataType) where

import Language.F2.DataType
import Language.F2.Parser
import Language.F2.TypeInterface
import Language.F2.Eval

import Control.Monad.Error

version = "0.1.2.0 (2013/07/17)"

defaultEnv :: Env
defaultEnv = []

preludeEnv :: Env
preludeEnv = [
  ("id", ((TFun (TVar "'a") (TVar "'a")),
         (VFFI (\x -> return $ x)))),
  ("~", ((TFun TInt TInt),
         (VFFI (\(VInt x)-> return $ VInt (negate x))))),
  ("+", ((TFun TInt (TFun TInt TInt)),
         (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> return $ VInt (x + y)))))),
  ("-", ((TFun TInt (TFun TInt TInt)),
         (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> return $ VInt (x - y)))))),
  ("*", ((TFun TInt (TFun TInt TInt)),
         (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> return $ VInt (x * y)))))),
  ("/", ((TFun TInt (TFun TInt TInt)),
         (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> if y /= 0 then return $ VInt (x `div` y) else lift $ Left "exec error : 0 div"))))),
  ("==", ((TFun TInt (TFun TInt TBool)),
          (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> return $ VBool (x == y)))))),
  ("/=", ((TFun TInt (TFun TInt TBool)),
          (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> return $ VBool (x /= y)))))),
  ("<", ((TFun TInt (TFun TInt TBool)),
          (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> return $ VBool (x < y)))))),
  (">", ((TFun TInt (TFun TInt TBool)),
          (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> return $ VBool (x > y)))))),
  ("<=", ((TFun TInt (TFun TInt TBool)),
          (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> return $ VBool (x <= y)))))),
  (">=", ((TFun TInt (TFun TInt TBool)),
          (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> return $ VBool (x >= y))))))]

exec :: Env -> String -> Either String (Type, Value)
exec env src = do
  ast <- parse src
  (_, t, _) <- tinf (toTyEnv env) ast
  v <- eval (toVEnv env) ast
  return (t, v)

execPrelude = exec preludeEnv

toTyEnv = map (\(s, (t, _))-> (s, t))
toVEnv = map (\(s, (_, v))-> (s, v))
