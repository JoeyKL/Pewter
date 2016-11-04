{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser where

import qualified Data.Set                   as E
import qualified Data.Text                  as T
import qualified Error
import           Text.Megaparsec            (many, parse)
import qualified Text.Megaparsec            as Megaparsec
import           Text.Megaparsec.Combinator (between, choice, endBy, manyTill)
import qualified Text.Megaparsec.Prim       as Prim
import qualified Token

type Parser a = Megaparsec.Parsec Megaparsec.Dec [Token.Token] a

data Declaration
  = ValueDeclaration Identifier Expr
  | TypeSignatureDeclaration Identifier TypeExpr
  | TypeDeclaration Identifier [Identifier] [TypeConstructor]
  deriving (Show)

data TypeConstructor = TypeConstructor Identifier [TypeExpr]
  deriving (Show)


data TypeExpr = TypeVariable Identifier | TypeApplication TypeExpr TypeExpr
  deriving (Show)

data Expr
  = Lambda [Identifier] Expr
  | Application Expr Expr
  | Variable Identifier
  | LetClause [LetStatement] Expr
  deriving (Show)

data LetStatement = LetStatement Identifier Expr
  deriving (Show)

data Identifier = Identifier T.Text
  deriving (Show)

instance Prim.Stream [Token.Token] where
  type Token [Token.Token] = Token.Token

  uncons (t:ts) = Just (t, ts)
  uncons []     = Nothing

  updatePos _ _ _ _ = (Megaparsec.initialPos "fuk u no err msg",Megaparsec.initialPos "fuk u no err msg")

match :: Token.Token -> Parser Token.Token
match t = satisfy (\x -> if x == t then Just x else Nothing)

satisfy :: (Token.Token -> Maybe a) -> Parser a
satisfy f = Prim.token testChar Nothing
  where
    testChar x =
      case f x of
        Just y  -> Right y
        Nothing -> Left (E.empty, E.empty, E.empty)

main :: [Token.Token] -> Error.CompilerResult [Declaration]
main source = case parse program "Input tokens" source of
  Left err     -> Error.Failure (Error.ParseError err)
  Right result -> Error.Success result

program :: Parser [Declaration]
program = many declaration

declaration :: Parser Declaration
declaration = do
  dec <- choice
    [valueDeclaration
    ,typeDeclaration
    ,typeSignatureDeclaration
    ]
  match Token.Semicolon
  return dec

valueDeclaration :: Parser Declaration
valueDeclaration = do
  name <- identifier
  match Token.Equals
  value <- expr
  return $ ValueDeclaration name value

typeDeclaration :: Parser Declaration
typeDeclaration = do
  name <- identifier
  typeParams <- manyTill identifier (match Token.TypeEquals)
  extraConstructors <- endBy constructor (match Token.TypeOr)
  firstConstructor <- constructor
  return $ TypeDeclaration name typeParams (firstConstructor:extraConstructors)

typeExpr :: Parser TypeExpr
typeExpr = choice
  [TypeApplication <$> typeExpr <*> typeExpr
  ,TypeVariable <$> identifier
  ]

constructor :: Parser TypeConstructor
constructor = do
  name <- identifier
  params <- many typeExpr
  return $ TypeConstructor name params

typeSignatureDeclaration :: Parser Declaration
typeSignatureDeclaration = do
  name <- identifier
  match Token.TypeSignature
  typeOfValue <- typeExpr
  return $ TypeSignatureDeclaration name typeOfValue

expr :: Parser Expr
expr = choice
  [lambda
  ,application
  ,variable
  ,letClause
  ,expr `enclosedBy` Token.Paren
  ]

enclosedBy :: Parser a -> (Token.BracketKind -> Token.Token) -> Parser a
enclosedBy parser bracket = between (match $ bracket Token.Open) (match $ bracket Token.Close) parser

application :: Parser Expr
application = Application <$> expr <*> expr

identifier :: Parser Identifier
identifier = Identifier <$> satisfy validate
  where
    validate x = case x of
      Token.Identifier text -> Just text
      _                     -> Nothing

variable :: Parser Expr
variable = Variable <$> identifier

lambda :: Parser Expr
lambda = do
  match Token.LambdaStart
  parameters <- manyTill identifier (match Token.Arrow)
  value <- expr
  return $ Lambda parameters value

letClause :: Parser Expr
letClause = letBody `enclosedBy` Token.Brace where
  letBody = do
    statements <- endBy letStatement (match Token.Semicolon)
    value <- expr
    return $ LetClause statements value

letStatement :: Parser LetStatement
letStatement = do
  match Token.Let
  name <- identifier
  match Token.Equals
  value <- expr
  return $ LetStatement name value
