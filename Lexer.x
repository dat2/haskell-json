-- best resource for this
-- https://github.com/dagit/happy-plus-alex

{
{-# OPTIONS_GHC -w #-}

module Lexer
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , unLex
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  ) where

import Prelude hiding (lex)
import Control.Monad ( liftM )
}

%wrapper "monadUserState"

$digit = 0-9        -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

$hex = [0-9a-fA-F]
$escape = [\"\\\/bfnrt]

@string = \\$escape | "\u" $hex{4} | [^\"\n]

@int = $digit | [1-9] $digit+ | "-"$digit | "-"[1-9] $digit+
@frac = . $digit*
@exp = [eE][\+\-] $digit+
@number = @int | @int @frac | @int @exp | @int @frac @exp

tokens :-
    $white+                 ;
    "//".*                  ;
    "{"                     { lex' TLeftCurley  }
    "}"                     { lex' TRightCurley }
    "["                     { lex' TLeftSquare }
    "]"                     { lex' TRightSquare }
    \:                      { lex' TColon }
    \,                      { lex' TComma }
    \" @string* \"          { lex (TString . init . tail) }
    "true"                  { lex' TTrue }
    "false"                 { lex' TFalse }
    "null"                  { lex' TNull }
    @number                 { lex (TNumber . read) }
{

-- the user state
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- the token type
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass =
  TLeftCurley     |
  TRightCurley    |
  TLeftSquare     |
  TRightSquare    |
  TColon          |
  TComma          |
  TString String  |
  TTrue           |
  TFalse          |
  TNull           |
  TNumber Double  |
  TEOF
  deriving (Eq,Show)

-- For nice parser error messages.
unLex :: TokenClass -> String
unLex TLeftCurley = "{"
unLex TRightCurley = "}"
unLex TLeftSquare = "["
unLex TRightSquare = "]"
unLex TColon = ":"
unLex TComma = ","
unLex (TString s) = "\"" ++ s ++ "\""
unLex TTrue = "true"
unLex TFalse = "false"
unLex TNull = "null"
unLex (TNumber n) = show n
unLex TEOF = "<EOF>"

lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

lex' :: TokenClass -> AlexAction Token
lex' = lex . const

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TEOF

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
