{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Core
import qualified Data.Map
import qualified Data.Text   as T
import qualified Error
import qualified Lexer
import qualified Parser
import qualified TypeChecker
import System.IO

main :: String -> IO ()
main file =
  source <- readFile file
  let result = eval source
  putStrLn $ show result

compile :: T.Text -> Error.CompilerResult Core.Expr
compile source = do
  tokens <- Lexer.main source
  ast <- Parser.main tokens
  core <- Core.main ast
  return core

eval :: String -> Error.CompilerResult Core.Value
eval x = Core.eval Data.Map.empty <$> compile (T.pack x)
