module Main where

import qualified Data.Text     as T
import           Pewter.Lexer  as Lexer
import           Pewter.Parser as Parser

data CompilerResult a
  = Success a
  | Failure CompilerError

data CompilerError
  = ParseError

main :: IO ()
main = putStrLn "Yes, this the Pewter compiler, no, it doesn't work yet."

compile :: T.Text -> CompilerResult T.Text
compile =
  Lexer.lex >=>
  Parser.parse >=>
  undefined
