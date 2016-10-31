module Lexer where

import qualified Data.Text             as T
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

data Token
  = Identifier T.Text
  | Operator T.Text
  | Equals
  | TypeEquals
  | TypeOr
  | EndOfLine

lex :: T.Text -> CompilerError [Token]
