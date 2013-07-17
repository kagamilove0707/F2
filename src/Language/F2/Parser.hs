{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

module Language.F2.Parser (parse) where

import Text.Peggy (defaultDelimiter, peggy, parseString)
import Language.F2.DataType
import Language.F2.Util

[peggy|
space :: ()
  = [ \r\n\t] { () } / comment { () }

delimiter :: ()
  = [()\[\]<>;:,.+*/<>=:^~#$-'|&] { () }

comment :: ()
  = '{-' (space / (!"-}" . { () }))* '-}' { () }

top :: AST
  = expr !.

expr :: AST
  = letrecExpr / letExpr / funExpr / ifExpr / opExpr

letExpr :: AST
  = "let" ("(" op ")" / name) name+ "=" expr "in" expr { Let $1 (foldr (\a e->Fun a e) $3 $2) $4 }
  / "let" ("(" op ")" / name) "=" expr "in" expr { Let $1 $2 $3 }
  / "let" name op name "=" expr "in" expr { Let $2 (Fun $1 (Fun $3 $4)) $5 }

letrecExpr :: AST
  = "let" "rec" ("(" op ")" / name) name+ "=" expr "in" expr { LetRec $1 (foldr (\a e->Fun a e) $3 $2) $4 }
  / "let" "rec" name op name "=" expr "in" expr { LetRec $2 (Fun $1 (Fun $3 $4)) $5 }

funExpr :: AST
  = "fun" name+ "->" expr { foldr (\a e->Fun a e) $2 $1 }

ifExpr :: AST
  = "if" expr "then" expr "else" expr { If $1 $2 $3 }

opExpr :: AST
  = opExpr ("`" name "`" / op) appExpr { App (App (Var $2) $1) $3 }
  / appExpr

appExpr :: AST
  = appExpr value { App $1 $2}
  / value

value :: AST
  = "(" expr ")"
  / "(" expr ":" sig ")" { Sig $1 $2 }
  / "(" expr "," expr ")" { Tuple ($1, $2) }
  / "(" value ("`" name "`" / op) ")" { App (Var $2) $1 }
  / "(" ("`" name "`" / op) value ")" { Fun "" (App (App (Var $1) (Var "")) $2) }
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
  = !"fun" !"in" !"let" !"rec" !"if" !"then" !"else" [a-z_] [a-zA-Z0-9']* { $1 : $2 }
  / [~]+ { $1 }

op ::: String
  = [.+\-*/<>^~#$|&] [.+\-*/<>^~#$&|=:]* { $1 : $2 }
  / [=:] [.+\-*/<>^~#$&|=:]+ { $1 : $2 }
|]

parse :: String -> Either String AST
parse src = case parseString top "<source>" src of
  Left x -> Left $ "parse error : " ++ showParseError x
  Right x -> Right x
