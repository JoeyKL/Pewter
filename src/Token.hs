module Token where

import           Data.Text
import           Text.Megaparsec.Pos

data SourceToken = SourceToken Token (SourcePos, SourcePos)
  deriving (Eq, Ord)

instance Show SourceToken where
  show (SourceToken s (_,_)) = show s

data Token
  = Identifier Text
  | Constructor Text
  | IntegerLiteral Integer
  | BooleanLiteral Bool
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
  | Match
  deriving (Show, Eq, Ord)

data BracketKind
  = Open
  | Close
  deriving (Show, Eq, Ord)
