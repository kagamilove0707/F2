{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Parser (parse) where

import Text.Peggy (space, defaultDelimiter, peggy, parseString)
import DataType
import Util

[peggy|
top :: AST
  = expr !.

expr :: AST
  = letExpr / funExpr / ifExpr / appExpr

letExpr :: AST
  = "let" name "=" expr "in" expr { Let $1 $2 $3 }
  / "let" "(" op ")" "=" expr "in" expr { Let $1 $2 $3 }

funExpr :: AST
  = "fun" name+ "->" expr { foldr (\a e->Fun a e) $2 $1 }

ifExpr :: AST
  = "if" expr "then" expr "else" expr { If $1 $2 $3 }

appExpr :: AST
  = appExpr value { App $1 $2}
  / appExpr op appExpr { App (App (Var $2) $1) $3 }
  / value

value :: AST
  = "(" expr ")"
  / "(" expr "," expr ")" { Tuple ($1, $2) }
  / "(" op ")" { Var $1 }
  / intValue { IntLit $1 }
  / boolValue { BoolLit $1 }
  / name { Var $1 }

intValue ::: Integer
  = [1-9] [0-9]* { read ($1 : $2) }

boolValue ::: Bool
  = "True" { True }
  / "False" { False }

name ::: String
  = !"fun" !"in" !"let" !"if" !"then" !"else" [a-z_] [a-zA-Z0-9]* { $1 : $2 }

op ::: String
  = [+\-*/<>=:^~#$-]+
|]

parse :: String -> Either String AST
parse src = case parseString top "<source>" src of
  Left x -> Left $ showParseError x
  Right x -> Right x
