{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}


module Core where

import           Data.List
import           Data.Map    (Map)
import           Data.Map    as M
import           Data.Monoid (mempty, (<>))
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Error
import qualified Parser

newtype Name = Name Text
  deriving (Show, Eq, Ord)

data Expr
  = Lambda Name Expr
  | Application Expr Expr
  | Variable Name
  | TypeSignature Expr Type
  deriving (Show)

data Type
  = TypeVariable Name
  | TypeApplication Type Type
  | Function Type Type
  deriving (Show)

data Definition = Definition Name Expr

freeVariables :: Expr -> Set Name
freeVariables = \case
  (Lambda name expr)        -> S.delete name (freeVariables expr)
  (Application expr1 expr2) -> freeVariables expr1 <> freeVariables expr2
  (Variable name)           -> S.singleton name
  (TypeSignature expr _)    -> freeVariables expr

resolve :: [Definition] -> Maybe [Definition]
resolve (def:defs) = undefined

class Node a where
  type Label a
  label :: a -> Label a
  connections :: a -> [a]

order :: (Node n, Eq (Label n)) => [n] -> [Label n]
order (node : nodes) =
  let firstWithDepends = eliminateDuplicates (label node : order (connections node))
  in (Prelude.filter (`notElem` firstWithDepends) (order nodes) <> firstWithDepends)

eliminateDuplicates :: Eq a => [a] -> [a]
eliminateDuplicates []     = []
eliminateDuplicates (x:xs) = x : eliminateDuplicates (Prelude.filter (x /= ) xs)

program :: Parser.Program -> CompilerResult Parser.Expr
program (Parser.Program decs) = undefined
