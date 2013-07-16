{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Parser (parse) where

import Text.Peggy (defaultDelimiter, peggy, parseString)
import DataType
import Util

[peggy|
space :: ()
  = [ \r\n\t] { () } / comment { () }

comment :: ()
  = '{-' (space / (!"-}" . { () }))* '-}' { () }

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
  / "(" expr ":" sig ")" { Sig $1 $2 }
  / "(" expr "," expr ")" { Tuple ($1, $2) }
  / "(" op ")" { Var $1 }
  / intValue { IntLit $1 }
  / boolValue { BoolLit $1 }
  / name { Var $1 }

sig :: Type
  = sigPrim "->" sig { TFun $1 $2 }
  / sigPrim

sigPrim :: Type
  = "(" sig ")"
  / "(" sig "," sig ")" { TTuple ($1, $2) }
  / "Int" { TInt }
  / "Bool" { TBool }
  / '\'' sigName { TVar ('\'' : $1) }

sigName :: String
  = [a-z] [a-zA-Z]* { $1 : $2 }

intValue ::: Integer
  = [1-9] [0-9]* { read ($1 : $2) }
  / '0' { 0 }

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
