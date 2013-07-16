module Language.F2.Util (showParseError) where

import Text.Peggy

showParseError (ParseError (LocPos sp) mes)
  = locFile sp ++ "(" ++ show (locLine sp) ++ ":" ++ show (locCol sp) ++ ") " ++ mes

