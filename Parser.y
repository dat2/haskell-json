{
module Parser where

import Lexer
import Syntax

import Control.Monad.Except

import qualified Data.Map as M
}

%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TEOF }
%error { happyError }

%token
  '{'     { Token _ TLeftCurley }
  '}'     { Token _ TRightCurley }
  '['     { Token _ TLeftSquare }
  ']'     { Token _ TRightSquare }
  ':'     { Token _ TColon }
  ','     { Token _ TComma }
  'true'  { Token _ TTrue }
  'false' { Token _ TFalse }
  'null'  { Token _ TNull }
  STR     { Token _ (TString $$) }
  NUM     { Token _ (TNumber $$) }

%name json
%%

Json  : Object  { $1 }
      | Array   { $1 }

Object  : '{' Kvps '}'    { JsonObject (M.fromList $2) }
        | '{' '}'         { JsonObject (M.fromList []) }

Array   : '[' Values ']'  { JsonArray $2 }
        | '[' ']'         { JsonArray [] }

Value   : Object              { $1 }
        | Array               { $1 }
        | STR                 { JsonString $1 }
        | 'true'              { JsonBool True }
        | 'false'             { JsonBool False }
        | 'null'              { JsonNull }
        | NUM                 { JsonNumber $1 }
Values  : Value               { [$1] }
        | Value ',' Values    { $1:$3 }

Kvp   : STR ':' Value     { ($1,$3) }
Kvps  : Kvp               { [$1] }
      | Kvp ',' Kvps      { $1:$3 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseJson :: FilePath -> String -> Either String JsonValue
parseJson = runAlex' json

}
