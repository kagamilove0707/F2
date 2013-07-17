module Language.F2 (
  exec, execPrelude, preludeEnv, version,
  module Language.F2.DataType) where

import Language.F2.DataType
import Language.F2.Parser
import Language.F2.TypeInterface
import Language.F2.Eval

import Control.Monad.Error

version = "0.1.3.0 (2013/07/17)"

defaultEnv :: Env
defaultEnv = []

preludeEnv :: Env
preludeEnv = [
  ("fix", fix),
  ("const", const),
  ("$", ap),
  ("flip", flip),
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
          (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> return $ VBool (x >= y)))))),
  ("&&", ((TFun TBool (TFun TBool TBool)),
          (VFFI (\(VBool x)-> return $ VFFI (\(VBool y)-> return $ VBool (x && y)))))),
  ("||", ((TFun TBool (TFun TBool TBool)),
          (VFFI (\(VBool x)-> return $ VFFI (\(VBool y)-> return $ VBool (x || y))))))]
  where
  Right fix = exec [] "(let rec fix f x = f (fix f) x in fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b)"
  Right const = exec [] "let const = fun x y -> x in (const : 'a -> 'b -> 'a)"
  Right ap = exec [] "let ($) = fun f x -> f x in (($) : ('a -> 'b) -> 'a -> 'b)"
  Right flip = exec [] "let flip f y x = f x y in (flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c)"

exec :: Env -> String -> Either String (Type, Value)
exec env src = do
  ast <- parse src
  (_, t, _) <- tinf (toTyEnv env) ast
  v <- eval (toVEnv env) ast
  return (t, v)

execPrelude = exec preludeEnv

toTyEnv = map (\(s, (t, _))-> (s, t))
toVEnv = map (\(s, (_, v))-> (s, v))
