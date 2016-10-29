module Lexer where

import qualified Data.Text             as T
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text

lex :: T.Text -> Maybe [Token]
