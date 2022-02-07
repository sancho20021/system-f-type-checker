module Base
  ( Context
  , Deduce (..)
  , EVar
  , Expr (..)
  , MyError (..)
  , TVar
  , Type (..)
  , TypedExpr (..)
  , prettyCtx
  ) where
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)

type TVar = String

data Type
  = TVar TVar
  | Type :-> Type
  | Forall TVar Type
  deriving (Eq)

instance Show Type where
  show (TVar s)       = s
  show (TVar s :-> b) = s ++ " -> " ++ show b
  show (s :-> b)      = printf "(%s) -> %s" (show s) (show b)
  show (Forall x t)   = printf "forall %s. %s" x (show t)

type EVar = String

data Expr
  = EVar EVar
  | Expr :$ Expr
  | Expr :@ Type
  | ELam EVar Type Expr
  | TLam TVar Expr
  deriving (Eq)

instance Show Expr where
  show (EVar v)     = v
  show (x :@ t)     = printf "%s[%s]" (inParen x) (show t)
  show (a :$ b)     = printf "%s %s" (show a) (inParen b)
  show (ELam x t m) = printf "\\%s : %s. %s" x (show t) (show m)
  show (TLam a m)   = printf "/\\%s. %s" a (show m)

inParen :: Expr -> String
inParen (EVar var) = var
inParen y          = "(" ++ show y ++ ")"

-- | Context that contains type variables and typed expression variables
type Context = Map EVar Type


data TypedExpr = Expr ::: Type
  deriving (Eq)
instance Show TypedExpr where
  show (expr ::: typ) = printf "%s: %s" (show expr) (show typ)

-- | Statement that expression
-- has its type in specified ctx
data Deduce = Context :|- TypedExpr
  deriving (Eq)
instance Show Deduce where
  show (ctx :|- te) =
    if null ctx
    then "|- " ++ show te
    else show ctx ++ " |- " ++ show te

infix 4 :|-
infix 5 :::
infix 5 :@
infix 5 :$
infixr 6 :->

-- | Errors that occur while parsing or type-checking
data MyError
  = InContextError MyError Context Expr
  | CheckError String
  | ParseError String
  deriving (Eq)

instance Show MyError where
  show (CheckError s)                = printf "CheckError: %s" s
  show (ParseError s)                = printf "ParseError: %s" s
  show (InContextError err ctx expr) =
    printf "%s\n  in: %s |- %s : ?" (show err) (prettyCtx ctx) (show expr)

prettyCtx :: Context -> String
prettyCtx ctx =
  let showWithType (e, t) = e ++ ": " ++ show t
  in intercalate ", " (showWithType <$> Map.toList ctx)
