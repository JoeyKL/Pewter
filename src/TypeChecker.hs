{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module TypeChecker where

import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Core
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Monoid                hiding (Product, Sum)
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import qualified Data.Text                  as T


newtype Substitution = Sub (Map Name MonoType)

class Substitutable a where
  applySub :: Substitution -> a -> a

instance (Substitutable a, Substitutable b) => Substitutable (a,b) where
    applySub s (x,y) = (applySub s x, applySub s y)

instance Substitutable Substitution where
    applySub s (Sub target) = Sub (fmap (applySub s) target)

instance Substitutable MonoType where
  applySub (Sub s) (TypeVariable n) = M.findWithDefault (TypeVariable n) n s
  applySub sub     (TypeBuiltin c)  = TypeBuiltin c
  applySub sub     (Function f x)   = Function (applySub sub f) (applySub sub x)
  applySub sub     (Product a b)    = Product (applySub sub a) (applySub sub b)
  applySub sub     (Sum a b)        = Sum (applySub sub a) (applySub sub b)

instance Substitutable PolyType where
  applySub (Sub subst) (Forall qs mType) =
      let qs' = M.fromSet (const ()) qs
          subst' = Sub (subst `M.difference` qs')
      in Forall qs (applySub subst' mType)

instance Monoid Substitution where
  mappend sub1 sub2 = Sub (s1 `M.union` s2)
    where
      Sub s1 = sub1
      Sub s2 = applySub sub1 sub2
  mempty = Sub M.empty

newtype Environment = Env (Map Name PolyType)

instance Free Environment where
  freeVariables (Env env) = S.unions (map freeVariables (M.elems env))

instance Substitutable Environment where
  applySub s (Env env) = Env (M.map (applySub s) env)

newtype Infer a = Infer (ExceptT InferError (State [Name]) a)
  deriving (Functor, Applicative, Monad)

data InferError
  = CannotUnify MonoType MonoType
  | OccursCheckFailed Name MonoType
  | UnknownIdentifier Name
  deriving (Show)

runInfer :: Infer a -> Either InferError a
runInfer (Infer inf) =
    evalState (runExceptT inf) (infiniteSupply alphabet)
  where
    alphabet = map T.singleton ['a'..'z']
    infiniteSupply alphabet = alphabet <> addSuffixes alphabet (1 :: Integer)
      where
        addSuffixes xs n = map (\x -> addSuffix x n) xs <> addSuffixes xs (n+1)
        addSuffix x n = x <> T.pack (show n)

throw :: InferError -> Infer a
throw = Infer . throwE

unify :: (MonoType, MonoType) -> Infer Substitution
unify = \case
  (Function a b, Function x y)            -> unifyBinary (a,b) (x,y)
  (TypeVariable v, x)                     -> v `bindVariableTo` x
  (x, TypeVariable v)                     -> v `bindVariableTo` x
  (TypeBuiltin a, TypeBuiltin b) | a == b -> return mempty
  (Sum a b, Sum x y)                      -> unifyBinary (a,b) (x,y)
  (Product a b, Product x y)              -> unifyBinary (a,b) (x,y)
  (a, b)                                  -> throw (CannotUnify a b)

unifyBinary :: (MonoType, MonoType) -> (MonoType, MonoType) -> Infer Substitution
unifyBinary (a,b) (x,y) = do
  s1 <- unify (a, x)
  s2 <- unify (applySub s1 (b, y))
  return (s1 <> s2)

bindVariableTo :: Name -> MonoType -> Infer Substitution
bindVariableTo name (TypeVariable v)
  | name == v = return mempty
bindVariableTo name monoType
  | name `S.member` freeVariables monoType = throw (OccursCheckFailed name monoType)
  | otherwise = return (Sub (M.singleton name monoType))



fresh :: Infer MonoType
fresh = drawFromSupply >>= \case
    Right name -> return (TypeVariable name)
    Left err -> throw err
  where
    drawFromSupply :: Infer (Either InferError Name)
    drawFromSupply = Infer (do
      s:supply <- lift get
      lift (put supply)
      return (Right s)
      )

extendEnv :: (Name, PolyType) -> Environment -> Environment
extendEnv (name, polyType) (Env env) = Env (M.insert name polyType env)

infer :: Environment -> Expr -> Infer (Substitution, MonoType)
infer env = \case
  Variable name -> inferVar env name
  Application f x -> inferApp env f x
  Lambda e1 e2 -> inferLam env e1 e2

inferVar :: Environment -> Name -> Infer (Substitution, MonoType)
inferVar env name = do
  sigma <- lookupEnv env name
  tau <- instantiate sigma
  return (mempty, tau)

lookupEnv :: Environment -> Name -> Infer PolyType
lookupEnv (Env env) name = case M.lookup name env of
  Just x  -> return x
  Nothing -> throw (UnknownIdentifier name)

instantiate :: PolyType -> Infer MonoType
instantiate (Forall qs t) = do
  sub <- substituteAllWithFresh qs
  return $ applySub sub t
    where
      substituteAllWithFresh :: Set Name -> Infer Substitution
      substituteAllWithFresh xs = do
        let freshSubActions = M.fromSet (const fresh) xs
        freshSubs <- sequenceA freshSubActions
        return (Sub freshSubs)

inferApp :: Environment -> Expr -> Expr -> Infer (Substitution, MonoType)
inferApp env f x = do
  (s1, fTau) <- infer env f
  (s2, xTau) <- infer (applySub s1 env) x
  fxTau <- fresh
  s3 <- unify (applySub s2 fTau, Function xTau fxTau)
  let s = s3 <> s2 <> s1
  return (s, applySub s3 fxTau)

inferLam :: Environment -> Name -> Expr -> Infer (Substitution, MonoType)
inferLam env x e = do
  tau <- fresh
  let sigma = Forall S.empty tau
  let env' = extendEnv (x, sigma) env
  (s, tau') <- infer env' e
  return (s, Function (applySub s tau) tau')
