{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Main where

import F2
import Util
import System.IO (hFlush, stdout)
import Control.Arrow (first, second, (>>>))
import Control.Monad.State
import Text.Peggy (space, defaultDelimiter, peggy, parseString)

[peggy|

top :: (String, String)
  = expr !.

expr :: (String, String)
  = defExpr
  / otherExpr { ("it", $1) }

defExpr :: (String, String)
  = "def" name "=" .+ { ($1, $2) }
  / "def" "(" op ")" "=" .+ { ($1, $2) }

otherExpr :: String
  = .+

name ::: String
  = !"fun" !"in" !"let" !"if" !"then" !"else" [a-z_] [a-zA-Z0-9]* { $1 : $2 }

op ::: String
  = [+\-*/<>=:^~#$-]+
|]

main = do
  putStrLn $ "hello. F2 v" ++ version ++ "\n"
  runStateT mainloop (1, preludeEnv)

mainloop :: StateT (Int, Env) IO ()
mainloop = do
  (n, env) <- get
  lift $ putStr $ "(" ++ show n ++ ")# "
  lift $ hFlush stdout
  line <- lift $ getLine
  if line == ":q" then do
    lift $ putStrLn "\n  See you!!"
  else do
    case parseString top "<source>" line of
      Left e -> lift $ putStrLn $ "  parse error : " ++ showParseError e
      Right (name, x) -> case exec env x of
        Left e -> lift $ putStrLn $ "  " ++ e
        Right (t, v) -> do
          modify (second ((name, (t, v)):))
          lift $ putStrLn $ "  " ++ name ++ " = " ++ x ++ " = " ++ show v ++ " : " ++ show t
    modify (first (+ 1))
    mainloop
