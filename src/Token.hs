module Token where

import           Data.Text

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
