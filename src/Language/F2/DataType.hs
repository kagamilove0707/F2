module Language.F2.DataType (
  Name,
  AST (..),
  Type (..),
  Value (..),
  TyEnv, TySubst, Env, VEnv
  )where

import Control.Monad.State

type Name = String

data AST
  = IntLit Integer
  | BoolLit Bool
  | Var Name
  | Tuple (AST, AST)
  | Fun Name AST
  | App AST AST
  | If AST AST AST
  | Let Name AST AST
  | Sig AST Type deriving (Show)

data Type
  = TInt
  | TBool
  | TTuple (Type, Type)
  | TVar Name
  | TFun Type Type deriving (Eq)

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show (TTuple (t1, t2)) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show (TVar s) = s
  show (TFun t1 t2) = show t1 ++ " -> " ++ show t2

data Value
  = VInt Integer
  | VBool Bool
  | VTuple (Value, Value)
  | VFun VEnv AST
  | VFFI (Value -> StateT VEnv (Either String) Value)

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show (VTuple xy) = show xy
  show (VFun _ _) = "<fun>"
  show (VFFI _) = "<fun>"

type Env = [(String, (Type, Value))]

type VEnv = [(String, Value)]

type TyEnv = [(String, Type)]

type TySubst = [(Name, Type)]
