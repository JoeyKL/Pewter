{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}


module Core where

import           Control.Monad
import           Data.List
import qualified Data.List          as L
import           Data.List.NonEmpty
import           Data.Map.Strict    (Map)
import           Data.Map.Strict    as M
import           Data.Maybe
import           Data.Monoid        (mempty, (<>))
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Debug.Trace
import           Error
import qualified Parser

type Name = Text

data Expr
  = Lambda Name Expr
  | Application Expr Expr
  | Variable Name
  | Typed Expr PolyType
  | Literal Literal
  | MatchClause Expr [Case]
  | Construction Name [Expr]
  deriving (Show)

pretty = \case
  Lambda name expr -> "\\" <> name <> "." <> (pretty expr)
  Application e1 e2 -> "(" <> pretty e1 <> ")(" <> pretty e2 <> ")"
  Variable name -> name
  Literal (Integer x) -> T.pack $ show x
  Literal (Boolean x) -> T.pack $ show x
  MatchClause e cases -> "match (" <> pretty e <> ") {" <> (foldMap prettyCase cases) <> "}"
    where prettyCase (Case name names expr) = name <> foldMap id names <> "->" <> pretty expr
  Construction name exprs -> "[" <> name <> (foldMap pretty exprs) <> "]"

beaut = \case
  Fun name _ expr -> "%" <> name <> "." <> pretty expr
  Con name vals -> "[" <> name <> (foldMap beaut vals) <> "]"
  RuntimeError err -> "<ERR: " <> err <> ">"

data Value
  = Fun Name (Map Name Value) Expr
  | Con Name [Value]
  | RuntimeError T.Text
  deriving (Show)

data Case = Case Name [Name] Expr
  deriving (Show)

eval :: Map Name Value -> Expr -> Value
eval env x = let
  fuck = \case
    Lambda name body -> Fun name (M.delete name env) body
    Application e1 e2 -> case eval env e1 of
      Fun name bindings body -> eval (M.insert name (eval env e2) (M.union bindings env)) body
      Con _ _       -> RuntimeError "Cannot apply a construct to arguments"
      err           -> err
    e@(Variable name) -> case M.lookup name env of
      Just x  -> x
      Nothing -> RuntimeError $
        "Undefined variable: " <> name <> " in env "
          <> T.pack (show $ L.map fst (M.toList env))
          <> "when trying to evaluate" <> pretty e
    Typed e _ -> eval env e
    Literal (Integer 0) -> Con "ZERO" []
    Literal (Integer x) -> Con "SUCC" [eval env (Literal (Integer (x-1)))]
    Literal (Boolean True) -> Con "TRUE" []
    Literal (Boolean False) -> Con "FALSE" []
    MatchClause e cases -> case eval env e of
      Con name args ->
        let
          Just (Case _ params body) = L.find (\(Case caseName params _) -> (name == caseName) && (L.length args == L.length params)) cases
          applyEach args params = (M.fromList (L.zip params args)) `M.union` env
        in
          eval (applyEach args params) body
      Fun _ _ _ -> RuntimeError "Cannot match a function"
      err -> err
    Construction name es -> Con name (fmap (eval env) es)
  val = fuck x
  in if doTrace then trace ("env: " <> show (L.map (\(a,b) -> (a,T.unpack $ beaut b)) $ M.toList env) <> " val: " <> (T.unpack $ pretty x)) val
    else val

doTrace = False

data Literal
  = Integer Integer
  | Boolean Bool
  deriving (Show)

data MonoType
  = TypeVariable Name
  | TypeBuiltin Name
  | TypeApplication MonoType MonoType
  | Function MonoType MonoType
  | TypeConstructs (NonEmpty TypeConstruct)
  deriving (Show)

data TypeConstruct = Construct Name [MonoType]
  deriving (Show)

data PolyType = Forall (Set Name) MonoType
  deriving (Show)

instance Free MonoType where
  freeVariables = \case
    TypeVariable v -> S.singleton v
    TypeBuiltin _ -> S.empty
    Function d r -> freeVariables d <> freeVariables r

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
    (Literal _) -> S.empty
    (MatchClause expr cases) -> S.union (freeVariables expr) (S.unions $ L.map freeVariables cases)
    (Construction name exprs) -> (S.unions $ L.map freeVariables exprs)

instance Free Case where
  freeVariables (Case name ids expr) = S.difference (freeVariables expr) (S.fromList ids)

instance Free Definition where
  freeVariables (Definition _ expr) = freeVariables expr

resolve :: Map Name Expr -> CompilerResult [Definition]
resolve defs = do
  namesOrdered <- orderDependencies defs (keys defs) [] []
  mapM normalize (fmap (\n -> Definition n <$> M.lookup n defs) namesOrdered)
    where
      normalize (Just x) = Success x
      normalize Nothing  = Failure IFuckedUp

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

nodeThenDependencies :: (Graph g, Ord (Node g), Show (Node g)) => g -> Node g -> [Node g] -> [Node g] -> CompilerResult [Node g]
nodeThenDependencies graph node fulfilled excluded
  | node `elem` excluded  = Failure RecursiveDefinitions
  | node `elem` fulfilled = Success []
  | otherwise = do
      edges <- case connections graph node of
        Just a  -> Success a
        Nothing -> Failure (UnknownIdentifier (T.pack $ show node))
      dependencies <- orderDependencies graph edges fulfilled (node:excluded)
      return $ node:dependencies

orderDependencies :: (Graph g, Ord (Node g), Show (Node g)) => g -> [Node g] -> [Node g] -> [Node g] -> CompilerResult [Node g]
orderDependencies graph edges fulfilled excluded = Data.List.foldr walkDependencies (Success []) edges
  where
    walkDependencies dep (Success newFulfilled) =
      fmap (newFulfilled <>) (nodeThenDependencies graph dep (fulfilled `L.union` newFulfilled) excluded)
    walkDependencies _   err                    = err

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
  Parser.Literal (Parser.Integer x) -> Literal (Integer x)
  Parser.Literal (Parser.Boolean x) -> Literal (Boolean x)
  Parser.MatchClause e cases -> MatchClause (expression e) (L.map convertCase cases)
    where convertCase (Parser.Case name bindings e) = Case name bindings (expression e)
  Parser.Construction name es -> Construction name (L.map expression es)


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
  orderedDefs <- L.reverse <$> resolve defMap
  return $ assembleDefinitions orderedDefs
