module Language.F2 (
  exec, execPrelude, preludeEnv, version,
  module Language.F2.DataType) where

import Language.F2.DataType
import Language.F2.Parser
import Language.F2.TypeInterface
import Language.F2.Eval
import Language.F2.Util

import Control.Applicative
import Control.Monad.Error

version = "0.1.3.4 20130718"

defaultEnv :: Env
defaultEnv = []

preludeEnv :: Env
preludeEnv = [
  ("fix", fix),
  ("const", const),
  ("$", ap),
  (".", compose),
  ("flip", flip),
  ("on", on),
  ("id", ((TFun (TVar "'a") (TVar "'a")),
         (VFFI (\x -> return $ x)))),
  ("~", ((TFun TInt TInt),
         (VFFI (\(VInt x)-> return $ VInt (negate x))))),
  ("+", ((TFun TInt (TFun TInt TInt)),
         (op (+) VInt getInt getInt))),
  ("-", ((TFun TInt (TFun TInt TInt)),
         (op (-) VInt getInt getInt))),
  ("*", ((TFun TInt (TFun TInt TInt)),
         (op (*) VInt getInt getInt))),
  ("/", ((TFun TInt (TFun TInt TInt)),
         (VFFI (\(VInt x)-> return $ VFFI (\(VInt y)-> if y /= 0 then return $ VInt (x `div` y) else throwError "exec error : 0 div"))))),
  ("==", ((TFun TInt (TFun TInt TBool)),
          (op (==) VBool getInt getInt))),
  ("/=", ((TFun TInt (TFun TInt TBool)),
          (op (/=) VBool getInt getInt))),
  ("<", ((TFun TInt (TFun TInt TBool)),
          (op (<) VBool getInt getInt))),
  (">", ((TFun TInt (TFun TInt TBool)),
          (op (>) VBool getInt getInt))),
  ("<=", ((TFun TInt (TFun TInt TBool)),
          (op (<=) VBool getInt getInt))),
  (">=", ((TFun TInt (TFun TInt TBool)),
          (op (>=) VBool getInt getInt))),
  ("&&", ((TFun TBool (TFun TBool TBool)),
          (VFFI $ \x-> return $ VFFI $ \y -> do
            x' <- getBool x
            if x'
              then do
                y' <- getBool y
                return $ VBool y'
              else return $ VBool False))),
  ("||", ((TFun TBool (TFun TBool TBool)),
                    (VFFI $ \x-> return $ VFFI $ \y -> do
            x' <- getBool x
            if x'
              then return $ VBool True
              else do
                y' <- getBool y
                return $ VBool y'))),
  ("fst", ((TFun (TTuple (TVar "'a", TVar "'b")) (TVar "'a")),
           (VFFI $ \x -> getTuple x >>= return . fst))),
  ("snd", ((TFun (TTuple (TVar "'a", TVar "'b")) (TVar "'b")),
           (VFFI $ \x -> getTuple x >>= return . snd)))]
  where
  op :: (a -> b -> c) -> (c -> Value) -> (Value -> Either String a) -> (Value -> Either String b) -> Value
  op f ret a1 a2 = (VFFI (\x-> return $ VFFI (\y-> do
    x' <- a1 x
    y' <- a2 y
    return $ ret (f x' y'))))
  Right fix = exec [] "(let rec fix f x = f (fix f) x in fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b)"
  Right const = exec [] "let const = fun x y -> x in (const : 'a -> 'b -> 'a)"
  Right ap = exec [] "let ($) = fun f x -> f x in (($) : ('a -> 'b) -> 'a -> 'b)"
  Right flip = exec [] "let flip f y x = f x y in (flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c)"
  Right compose = exec [] "let f . g = fun x -> f (g x) in ((.) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c)"
  Right on = exec [] "let on op f = fun x y -> f x `op` f y in (on : ('b -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'a -> 'c)"

exec :: Env -> String -> Either String (Type, Value)
exec env src = do
  ast <- parse src
  (_, t, _) <- tinf (toTyEnv env) ast
  v <- eval (toVEnv env) ast
  return (t, v)

execPrelude = exec preludeEnv

toTyEnv = map (\(s, (t, _))-> (s, t))
toVEnv = map (\(s, (_, v))-> (s, v))
