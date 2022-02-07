module Checker
  ( TErr
  , freeTVars
  , inferType
  , isInFV
  , typeCheck
  ) where
import Base (Context, Expr (ELam, EVar, TLam, (:$), (:@)), MyError (CheckError, InContextError),
             TVar, Type (Forall, TVar, (:->)), TypedExpr ((:::)))
import Control.Monad (when)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf (printf)

type TErr a = Either MyError a

checkError :: String -> TErr a
checkError = Left . CheckError

-- | This functinon performs type-check,
-- and in case of failure, puts context and expression into MyError.
-- This helps to understand, where exactly the problem in type-checking occurred
typeCheckTraced :: Context -> Expr -> TErr Type
typeCheckTraced ctx expr = case typeCheck' ctx expr of
  Left err -> Left $ InContextError err ctx expr
  Right ty -> return ty

typeCheck' :: Context -> Expr -> TErr Type
typeCheck' ctx expr = case expr of
  -- Variable must be in context
  EVar v -> case Map.lookup v ctx of
    Nothing -> checkError $ printf "Undefined var: %s" v
    Just ty -> return ty
  -- type-check both parts of application
  -- and ensure that their types match each other
  f :$ x -> do
    funType <- typeCheckTraced ctx f
    argType <- typeCheckTraced ctx x
    case funType of
      argType' :-> resType | argType' == argType -> return resType
      _                                          -> checkError $ errMsg funType argType
      where
        errMsg fType argType = printf "expr with type %s\ncan't be applied to expr with type %s"
          (show fType) (show argType)
  -- ensure that inner expression has type "forall a ..."
  ex :@ refined -> do
    exType <- typeCheckTraced ctx ex
    case exType of
      Forall a ty -> return $ subsType a refined ty
      _           -> checkError "Invalid type application"
  -- check that binding variable is not present in context
  ELam x ty ex -> do
    case Map.lookup x ctx of
      Nothing  -> return ()
      Just ty' -> checkError $ printf "Conflicting types of %s: %s, %s" x (show ty) (show ty')
    retType <- typeCheckTraced (Map.insert x ty ctx) ex
    return $ ty :-> retType
  -- check that the type variable is not contained in the set of 
  -- free type variables of the context
  TLam a ex -> do
    when (foldl' (||) False (isInFV a <$> Map.elems ctx)) $
      checkError $ printf "Type variable %s is in FV(Context)" a
    innerType <- typeCheckTraced ctx ex
    return $ Forall a innerType

isInFV :: TVar -> Type -> Bool
isInFV x typ = x `Set.member` freeTVars typ

freeTVars :: Type -> Set TVar
freeTVars (TVar x)     = Set.singleton x
freeTVars (a :-> b)    = freeTVars a `Set.union` freeTVars b
freeTVars (Forall a t) = a `Set.delete` freeTVars t

-- | Perform type substitution
subsType :: TVar -> Type -> Type -> Type
subsType a refined = subsType'
  where
    subsType' :: Type -> Type
    subsType' typ = case typ of
      TVar a' | a == a'      -> refined
      TVar a'                -> TVar a'
      ty :-> ty'             -> subsType' ty :-> subsType' ty'
      Forall a' ty | a == a' -> Forall a' ty
      Forall a' ty           -> Forall a' (subsType' ty)

typeCheck
  :: Context
  -> TypedExpr
  -> TErr Type
typeCheck ctx (expr ::: actT) = case typeCheckTraced ctx expr of
  Left s -> Left s
  Right expT | expT == actT -> return expT
  Right expT -> checkError $
    printf "Invalid type.\nExpected: %s\n actual: %s" (show expT) (show actT)

inferType :: Context -> Expr -> TErr Type
inferType = typeCheckTraced
