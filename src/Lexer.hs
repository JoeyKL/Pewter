module Lexer where

import           Main
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
  | TypeSignature
  | Let
  | In
  | EndOfLine

lex :: T.Text -> CompilerError [Token]
lex = undefined

whitespace = L.space
