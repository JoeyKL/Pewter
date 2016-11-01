module Error where

import           Text.Megaparsec.Error as Parsec

data CompilerResult a
  = Success a
  | Failure CompilerError
  deriving (Show)

data CompilerError
  = ParseError (Parsec.ParseError Char Dec)
  deriving (Show)
