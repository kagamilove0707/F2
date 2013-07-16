module TypeInterface (
  tinf', tinf, unify
  ) where

import DataType
import Control.Monad.Error
import Control.Monad.State

unify :: [(Type, Type)] -> Either String TySubst
unify eqs = solve eqs []
  where
  solve [] th = Right th
  solve ((t1,t2):eqs) th
   |t1 == t2 = solve eqs th
   |otherwise = case (t1, t2) of
     (TFun t11 t12, TFun t21 t22) -> solve ((t11, t21):(t12, t22):eqs) th
     (TVar s, _)
      |occurs t1 t2 -> Left "type error : occurs miss"
      |otherwise -> solve (substEq [(s, t2)] eqs) (composeSubst [(s, t2)] th)
     (_, TVar s)
      |occurs t2 t1 -> Left "type error : occurs miss"
      |otherwise -> solve (substEq [(s, t1)] eqs) (composeSubst [(s, t1)] th)
     (_, _) -> Left "type error : no match"

occurs :: Type -> Type -> Bool
occurs t1 (TFun t21 t22) = occurs t1 t21 || occurs t1 t22
occurs t1 t2 = t1 == t2

substTy :: TySubst -> Type -> Type
substTy _ TInt = TInt
substTy _ TBool = TBool
substTy _ (TTuple x) = TTuple x
substTy st (TVar s) = f st s
  where
  f [] s' = TVar s'
  f ((s, t):st) s'
   |s == s' = t
   |otherwise = f st s'
substTy st (TFun t1 t2) = TFun (substTy st t1) (substTy st t2)

substEq :: TySubst -> [(Type, Type)] -> [(Type, Type)]
substEq st eqs = map (\(t1, t2)-> (substTy st t1, substTy st t2)) eqs

substTyEnv :: TySubst -> TyEnv -> TyEnv
substTyEnv st env = map (\(s, t) -> (s, substTy st t)) env

composeSubst :: TySubst -> TySubst -> TySubst
composeSubst st2 st1 = foldl (\st (s, t) -> case lookup s st of
  Just _ -> st
  Nothing -> (s, t):st) st2' st1
  where
  st2' = map (\(s, t) -> (s, substTy st1 t)) st2

newTVar :: StateT Int (Either String) Type
newTVar = do
  n <- get
  put (n+1)
  return (TVar $ "'t" ++ show n)

remove _ [] = []
remove s ((s', t):st)
 | s == s' = st
 | otherwise = (s', t) : remove s st

incrTy :: Type -> StateT Int (Either String) Type
incrTy (TFun t1 t2) = do
  t1' <- incrTy t1
  t2' <- incrTy t2
  return $ TFun t1' t2'
incrTy (TTuple (t1, t2)) = do
  t1' <- incrTy t1
  t2' <- incrTy t2
  return $ TTuple (t1', t2')
incrTy TInt = return TInt
incrTy TBool = return TBool
incrTy (TVar s) = do
  i <- get
  return $ TVar $ s ++ "_" ++ show i

tinf' :: TyEnv -> AST -> StateT Int (Either String) (TyEnv, Type, TySubst)
tinf' env (IntLit _) = return (env, TInt, [])
tinf' env (BoolLit _) = return (env, TBool, [])
tinf' env (Var s) = case lookup s env of
  Just t@(TVar _) -> return (env, t, [])
  Just t -> do
    t' <- incrTy t
    modify (+ 1)
    return (env, t', [])
  Nothing -> lift $ Left $ "type error : not found \"" ++ s ++ "\""
tinf' env (Fun s e) = do
  tv <- newTVar
  let env' = (s, tv):env
  (env'', t, th) <- tinf' env' e
  let tv' = substTy th tv
  return (remove s env'', TFun tv' t, th)
tinf' env (App e1 e2) = do
  (env', t1, th1) <- tinf' env e1
  (env'', t2, th2) <- tinf' env' e2
  t3 <- newTVar
  let t11 = substTy th2 t1
  th3 <- lift $ unify [(t11, TFun t2 t3)]
  let t3' = substTy th3 t3
  let env''' = substTyEnv th3 env''
  return (env''', t3', th3 +*+ th2 +*+ th1)
  where
  (+*+) = composeSubst
tinf' env (If c e1 e2) = do
  (env', tc, thc) <- tinf' env c
  thc' <- lift $ unify [(tc, TBool)]
  let env'' = substTyEnv thc' env'
  tv1 <- newTVar
  (env1, t1, th1) <- tinf' env'' e1
  (env2, t2, th2) <- tinf' env1 e2
  th3 <- lift $ unify [(tv1, t1),(tv1, t2)]
  let env2' = substTyEnv th3 env2
  let tv1' = substTy th3 tv1
  return (env2', tv1', th3 +*+ th2 +*+ thc')
  where
  (+*+) = composeSubst
tinf' env (Let s e1 e2) = do
  (env', t1, th1) <- tinf' env e1
  let env'' = (s, t1):env'
  (env'', t2, th2) <- tinf' env'' e2
  return (remove s env'', t2, composeSubst th2 th1)
tinf' env (Tuple (e1, e2)) = do
  (env1, t1, th1) <- tinf' env  e1
  (env2, t2, th2) <- tinf' env1 e2
  return (env1, TTuple (t1, t2), composeSubst th2 th1)
tinf' env (Sig ast t) = do
  (env', t1, th1) <- tinf' env ast
  th2 <- lift $ unify [(t1, t)]
  let t1' = substTy th2 t
  return (env', t1', composeSubst th2 th1)

tinf env e = evalStateT (tinf' env e) 0
