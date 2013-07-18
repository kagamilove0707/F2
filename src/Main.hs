{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Main where

import Language.F2
import Language.F2.Util
import System.IO (hFlush, stdout)
import Control.Arrow (first, second, (>>>))
import Control.Monad.State
import Text.Peggy (defaultDelimiter, peggy, parseString)

[peggy|
space :: ()
  = [ \r\n\t] { () } / comment { () }

delimiter :: ()
  = [()\[\]<>;:,.+*/<>=:^~#$-'|&] { () }

comment :: ()
  = '{-' (space / (!"-}" . { () }))* '-}' { () }

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
  = !"fun" !"in" !"let" !"rec" !"if" !"then" !"else" [a-z_] [a-zA-Z0-9~']* { $1 : $2 }
  / [~]+ { $1 }

op ::: String
  = [.+\-*/<>^~#$|&] [.+\-*/<>^~#$&|=:]* { $1 : $2 }
  / [=:] [.+\-*/<>^~#$&|=:]+ { $1 : $2 }
|]

helloStr
 = "    ____ _____\n" ++
   "  //        //\n" ++
   " //--   ----  \n" ++
   "//     //___  version " ++ version ++ "\n"

main = do
  putStrLn $ helloStr
  runStateT mainloop (1, preludeEnv)

mainloop :: StateT (Int, Env) IO ()
mainloop = do
  (n, env) <- get
  lift $ putStr $ "(" ++ show n ++ ")# "
  lift $ hFlush stdout
  line <- lift $ getLine
  if line == ":q" then do
    lift $ putStrLn "\n  See you!!"
  else if line == ":v" then do
    lift $ putStrLn $ "  version " ++ version
    modify (first (+ 1))
    mainloop
  else do
    case parseString top "<source>" line of
      Left e -> lift $ putStrLn $ "  parse error : " ++ showParseError e
      Right (name, x) -> case exec env x of
        Left e -> lift $ putStrLn $ "  " ++ e
        Right (t, v) -> do
          case getValue v of
            Left e -> lift $ putStrLn $ "  " ++ e
            Right v' -> do
              modify (second ((name, (t, v')):))
              lift $ putStrLn $ "  " ++ name ++ " = " ++ x ++ " = " ++ show v' ++ " : " ++ show t
    modify (first (+ 1))
    mainloop
