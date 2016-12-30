{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}


module Core where

import           Control.Monad
import           Data.List
import qualified Data.List       as L
import           Data.Map.Strict (Map)
import           Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid     (mempty, (<>))
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Error
import qualified Parser

type Name = Text

data Expr
  = Lambda Name Expr
  | Application Expr Expr
  | Variable Name
  | Typed Expr PolyType
  deriving (Show)

data MonoType
  = TypeVariable Name
  | TypeBuiltin Name
  | TypeApplication MonoType MonoType
  | Function MonoType MonoType
  | Product MonoType MonoType
  | Sum MonoType MonoType
  deriving (Show)

data PolyType = Forall (Set Name) MonoType
  deriving (Show)

instance Free MonoType where
  freeVariables = \case
    TypeVariable v -> S.singleton v
    TypeBuiltin _ -> S.empty
    Function d r -> freeVariables d <> freeVariables r
    Product a b -> freeVariables a <> freeVariables b
    Sum a b -> freeVariables a <> freeVariables b

instance Free PolyType where
  freeVariables (Forall names monoType) =
    S.difference (freeVariables monoType) names

data Definition = Definition Name Expr

class Free f where
  freeVariables :: f -> Set Name

instance Free Expr where
  freeVariables = \case
    (Lambda name expr)        -> S.delete name (freeVariables expr)
    (Application expr1 expr2) -> freeVariables expr1 <> freeVariables expr2
    (Variable name)           -> S.singleton name
    (Typed expr _)    -> freeVariables expr

instance Free Definition where
  freeVariables (Definition _ expr) = freeVariables expr

resolve :: Map Name Expr -> CompilerResult [Definition]
resolve defs = do
  namesOrdered <- normalize (orderDependencies defs (keys defs) [] [])
  mapM normalize (fmap (\n -> Definition n <$> M.lookup n defs) namesOrdered)
    where
      normalize :: Maybe a -> CompilerResult a
      normalize Nothing  = Failure RecursiveDefinitions
      normalize (Just x) = Success x

class Graph g where
  type Node g
  connections :: g -> Node g -> Maybe [Node g]

instance Graph (Map Name Expr) where
  type Node (Map Name Expr) = Name
  connections defs def = S.toList <$> freeVariables <$> M.lookup def defs

assembleDefinitions :: [Definition] -> Expr
assembleDefinitions (Definition n e :ds) =
  Application (Lambda n (assembleDefinitions ds)) e
assembleDefinitions [] = Variable "main"

nodeThenDependencies :: (Graph g, Ord (Node g)) => g -> Node g -> [Node g] -> [Node g] -> Maybe [Node g]
nodeThenDependencies graph node fulfilled excluded
  | node `elem` excluded  = Nothing
  | node `elem` fulfilled = Just []
  | otherwise = do
      edges <- connections graph node
      dependencies <- orderDependencies graph edges fulfilled (node:excluded)
      Just (node:dependencies)

orderDependencies :: (Graph g, Ord (Node g)) => g -> [Node g] -> [Node g] -> [Node g] -> Maybe [Node g]
orderDependencies graph edges fulfilled excluded = Data.List.foldr walkDependencies (Just []) edges
  where
    walkDependencies dep (Just newFulfilled) =
      fmap (newFulfilled <>) (nodeThenDependencies graph dep (fulfilled `L.union` newFulfilled) excluded)
    walkDependencies _    Nothing             = Nothing

decsToDefs :: Parser.Program -> CompilerResult (Map Name Expr)
decsToDefs (Parser.Program decs) = do
  decs <- foldM (flip addDec) M.empty decs
  mapM normalize decs
  where
    normalize :: Either Expr [PolyType] -> CompilerResult Expr
    normalize (Left expr) = Success expr
    normalize (Right _)   = Failure MissingDefinition

applyTypes :: [PolyType] -> Expr -> Expr
applyTypes ts e = L.foldr (flip Typed) e ts

addDec :: Parser.Declaration -> Map Name (Either Expr [PolyType]) -> CompilerResult (Map Name (Either Expr [PolyType]))
addDec (Parser.ValueDeclaration name expr) defs =
  case M.lookup name defs of
    Just (Left _)   -> Failure (DuplicateDeclaration name)
    Just (Right ts) -> Success (M.insert name (Left $ applyTypes ts $ expression expr) defs)
    Nothing         -> Success (M.insert name (Left $ expression expr) defs)
addDec (Parser.TypeSignatureDeclaration name parserType) defs =
  let theType = convertType parserType in
  case M.lookup name defs of
    Just (Left expr) -> Success (M.insert name (Left $ applyTypes [theType] expr) defs)
    Just (Right ts)  -> Success (M.insert name (Right $ theType:ts) defs)
    Nothing          -> Success (M.insert name (Right [theType]) defs)

expression :: Parser.Expr -> Expr
expression = \case
  Parser.Lambda (id:ids) e -> Lambda id $ expression $ Parser.Lambda ids e
  Parser.Lambda [] e -> expression e
  Parser.Application e1 e2 -> Application (expression e1) (expression e2)
  Parser.Variable id -> Variable id
  Parser.LetClause (Parser.LetStatement id e1 : sts) e2 ->
    Application (Lambda id (expression (Parser.LetClause sts e2))) (expression e1)
  Parser.LetClause [] e2 -> expression e2

convertType :: Parser.TypeExpr -> PolyType
convertType parsedType = Forall (freeVariables monoType) monoType where
  convertMonoType = \case
    Parser.TypeVariable id -> TypeVariable id
    Parser.TypeApplication t1 t2 -> TypeApplication (convertMonoType t1) (convertMonoType t2)
    Parser.Function d r -> Function (convertMonoType d) (convertMonoType r)
  monoType = convertMonoType parsedType

main :: Parser.Program -> CompilerResult Expr
main prog = do
  defMap <- decsToDefs prog
  orderedDefs <- resolve defMap
  return $ assembleDefinitions orderedDefs
