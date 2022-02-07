module Main
  ( main
  ) where
import Base (Expr (ELam, EVar, (:$)), Type (TVar), TypedExpr ((:::)))
import Checker (inferType)
import qualified CommonTest
import Control.Monad (forM_)
import qualified Data.Map as Map
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain CommonTest.tests

