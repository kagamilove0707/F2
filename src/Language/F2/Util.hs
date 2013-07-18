module Language.F2.Util (showParseError, getInt, getBool, getTuple, applyValue, getValue) where

import Language.F2.Eval
import Text.Peggy

showParseError (ParseError (LocPos sp) mes)
  = locFile sp ++ "(" ++ show (locLine sp) ++ ":" ++ show (locCol sp) ++ ") " ++ mes
