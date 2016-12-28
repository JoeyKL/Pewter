{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser where

import           Data.List.NonEmpty
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Error
import           Text.Megaparsec            (many, parse)
import qualified Text.Megaparsec            as Megaparsec
import           Text.Megaparsec.Combinator (between, choice, endBy, manyTill)
import           Text.Megaparsec.Error
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Prim       as Prim
import           Token                      (SourceToken (..))
import qualified Token

type Parser a = Megaparsec.Parsec Megaparsec.Dec [SourceToken] a

data Program = Program [Declaration]

data Declaration
  = ValueDeclaration Identifier Expr
  | TypeSignatureDeclaration Identifier TypeExpr
  | TypeDeclaration Identifier [Identifier] [TypeConstructor]
  deriving (Show)

data TypeConstructor = TypeConstructor Identifier [TypeExpr]
  deriving (Show)

data TypeExpr
  = TypeVariable Identifier
  | TypeApplication TypeExpr TypeExpr
  | Function TypeExpr TypeExpr
  deriving (Show)

data Expr
  = Lambda [Identifier] Expr
  | Application Expr Expr
  | Variable Identifier
  | LetClause [LetStatement] Expr
  deriving (Show)

data LetStatement = LetStatement Identifier Expr
  deriving (Show)

type Identifier = T.Text

instance Prim.Stream [SourceToken] where
  type Token [SourceToken] = SourceToken

  uncons (t:ts) = Just (t, ts)
  uncons []     = Nothing

  updatePos _ _ _ (SourceToken _ indexes) = indexes

match :: Token.Token -> Parser Token.Token
match expected = satisfy (\token ->
    if token == expected then Just token else Nothing
  )

satisfy :: (Token.Token -> Maybe a) -> Parser a
satisfy f = Prim.token testToken Nothing
  where
    testToken sourceToken@(SourceToken token _) =
      case f token of
        Just y  -> Right y
        Nothing -> Left (Set.singleton (Tokens (sourceToken:|[])), Set.empty, Set.empty)

main :: [SourceToken] -> Error.CompilerResult Program
main source = case parse program "Input tokens" source of
  Left err     -> Error.Failure (Error.ParseError err)
  Right result -> Error.Success result

program :: Parser Program
program = Program <$> many declaration

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
typeExpr = makeExprParser (choice
  [TypeApplication <$> typeExpr <*> typeExpr
  ,TypeVariable <$> identifier
  ]) [[InfixL (return TypeApplication)]]

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
expr = makeExprParser (choice
  [lambda
  ,variable
  ,letClause
  ,expr `enclosedBy` Token.Paren
  ]) [[InfixL (return Application)]]

enclosedBy :: Parser a -> (Token.BracketKind -> Token.Token) -> Parser a
enclosedBy parser bracket = between (match $ bracket Token.Open) (match $ bracket Token.Close) parser

application :: Parser Expr
application = Application <$> expr <*> expr

identifier :: Parser Identifier
identifier = satisfy validate
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
