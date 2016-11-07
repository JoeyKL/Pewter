module Main where

import qualified Data.Text as T
import qualified Error
import qualified Lexer
import qualified Parser

main :: IO ()
main = putStrLn "Yes, this the Pewter compiler, no, it doesn't work yet."

compile :: T.Text -> Error.CompilerResult [Parser.Declaration]
compile source = do
  tokens <- Lexer.main source
  Parser.main tokens
