module Main where

import qualified Core
import qualified Data.Text   as T
import qualified Error
import qualified Lexer
import qualified Parser
import qualified TypeChecker

main :: IO ()
main = putStrLn "Yes, this the Pewter compiler, no, it doesn't work yet."

compile :: T.Text -> Error.CompilerResult Core.Expr
compile source = do
  tokens <- Lexer.main source
  ast <- Parser.main tokens
  core <- Core.main ast
  TypeChecker.main core
  return core
