{-# LANGUAGE DeriveFunctor #-}

module Error where

import           Text.Megaparsec.Error as Parsec
import           Token                 (SourceToken)

data CompilerResult a
  = Success a
  | Failure CompilerError
  deriving (Show, Functor)

data CompilerError
  = LexError (Parsec.ParseError Char Dec)
  | ParseError (Parsec.ParseError SourceToken Dec)
  | NoMain
  deriving (Show)

instance Applicative CompilerResult where
  pure = Success

  Success f <*> Success x = Success (f x)
  Failure x <*> _         = Failure x
  Success _ <*> Failure x = Failure x

instance Monad CompilerResult where
  return = Success

  Success x >>= f = f x
  Failure x >>= f = Failure x
