module Lexer where

import           Control.Monad         (void)
import           Data.Functor
import qualified Data.Text             as T
import           Error
--import           Text.Megaparsec
import           Text.Megaparsec       hiding (Token, token)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text
import           Token

main :: T.Text -> CompilerResult [Token]
main source = case parse lexer "Input file" source of
  Left err     -> Failure (Error.LexError err)
  Right result -> Success result

lexer :: Parser [Token]
lexer = many (lexeme token)

blank = void spaceChar
lineComment = L.skipLineComment "//"
blockComment = L.skipBlockCommentNested "/*" "*/"

whitespace :: Parser ()
whitespace = L.space blank lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

token :: Parser Token
token = choice
  [word
  ,integerLiteral
  ,equals
  ,openParen
  ,closeParen
  ,openBrace
  ,closeBrace
  ,typeEquals
  ,typeOr
  ,semicolon
  ]

word :: Parser Token
word = reservedWords <$> parseWord where
  parseWord :: Parser String
  parseWord = (:) <$> letterChar <*> many alphaNumChar

integerLiteral :: Parser Token
integerLiteral = IntegerLiteral <$> L.integer

equals :: Parser Token
equals =  Equals <$ string "="

openParen :: Parser Token
openParen =  Paren Open <$ string "("

closeParen :: Parser Token
closeParen =  Paren Close <$ string ")"

openBrace :: Parser Token
openBrace =  Brace Open <$ string "{"

closeBrace :: Parser Token
closeBrace =  Brace Close <$ string "}"

typeEquals :: Parser Token
typeEquals = TypeEquals <$ string ":="

typeOr :: Parser Token
typeOr = TypeOr <$ string "|"

typeSignature :: Parser Token
typeSignature = TypeSignature <$ string ":"

lambdaStart :: Parser Token
lambdaStart = LambdaStart <$ string "\\"

arrow :: Parser Token
arrow = Arrow <$ string "->"

semicolon :: Parser Token
semicolon = Semicolon <$ string ";"

reservedWords :: String -> Token
reservedWords "let" = Let
reservedWords word  = Identifier $ T.pack word
