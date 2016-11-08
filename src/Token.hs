module Token where

import           Data.Text
import           Text.Megaparsec.Pos

data SourceToken = SourceToken Token (SourcePos, SourcePos)
  deriving (Show, Eq, Ord)

data Token
  = Identifier Text
  | IntegerLiteral Integer
  | Paren BracketKind
  | Brace BracketKind
  | Equals
  | TypeEquals
  | TypeOr
  | TypeSignature
  | LambdaStart
  | Arrow
  | Let
  | Semicolon
  deriving (Show, Eq, Ord)

data BracketKind
  = Open
  | Close
  deriving (Show, Eq, Ord)
