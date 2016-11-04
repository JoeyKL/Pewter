{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser where

import qualified Data.Set                   as E
import qualified Data.Text                  as T
import           Error
import qualified Lexer
import qualified Text.Megaparsec            as Megaparsec
import           Text.Megaparsec.Combinator (between)
import qualified Text.Megaparsec.Prim       as Prim

type Parser a = Megaparsec.Parsec Megaparsec.Dec [Lexer.Token] a

data Expr
  = Lambda [Identifier] Expr
  | Application Expr Expr
  | Variable Identifier

data Identifier = Identifier T.Text

instance Prim.Stream [Lexer.Token] where
  type Token [Lexer.Token] = Lexer.Token

match :: Lexer.Token -> Parser Lexer.Token
match t = satisfy (\x -> if x == t then Just x else Nothing)

satisfy :: (Lexer.Token -> Maybe a) -> Parser a
satisfy f = Prim.token testChar Nothing
  where
    testChar x =
      case f x of
        Just y  -> Right y
        Nothing -> Left (E.empty, E.empty, E.empty)

main :: [Lexer.Token] -> CompilerResult Expr
main = undefined

expr :: Parser Expr
expr = Megaparsec.choice
  [lambda
  ,application
  ,variable
  ,expr `enclosedBy` Lexer.Paren
  ]

enclosedBy :: Parser a -> (Lexer.BracketKind -> Lexer.Token) -> Parser a
enclosedBy parser bracket = between (match $ bracket Lexer.Open) (match $ bracket Lexer.Close) parser

application :: Parser Expr
application = Application <$> expr <*> expr

variable :: Parser Expr
variable = Variable . Identifier <$> satisfy validate
  where
    validate x = case x of
      Lexer.Identifier text -> Just text
      _                     -> Nothing

lambda :: Parser Expr
lambda = undefined
