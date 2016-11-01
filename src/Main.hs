module Main where

import qualified Data.Text as T
import           Error     as Error
import           Lexer     as Lexer

main :: IO ()
main = putStrLn "Yes, this the Pewter compiler, no, it doesn't work yet."

compile :: T.Text -> Error.CompilerResult T.Text
compile =
  Lexer.lex >=>
  Parser.parse >=>
  undefined
