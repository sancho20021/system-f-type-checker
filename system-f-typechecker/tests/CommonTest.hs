module CommonTest
  ( tests
  ) where
import Base (Context, Deduce ((:|-)), EVar, Expr (ELam, EVar, TLam, (:$), (:@)), MyError, TVar,
             Type (Forall, TVar, (:->)), TypedExpr ((:::)))
import Checker (TErr, freeTVars, typeCheck)
import Control.Monad (liftM2, void)
import Data.Either (isLeft, isRight)
import Data.Foldable (Foldable (foldl'), find)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Parser (parseWithPrettyError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@=?))
import Text.Printf (printf)

tests :: TestTree
tests = testGroup "Checker and parser tests" [ manualTests ]

manualTests :: TestTree
manualTests = testGroup "Manual Tests" [correct, incorrect]

correct :: TestTree
correct = testGroup "Correct typed expressions" $
  mkTestGroup <$> zip [0..] [ group0, group1, group2, group3, group4]
  where
    mkTestGroup :: (Int, [String]) -> TestTree
    mkTestGroup (n, group) = testGroup (printf "group %d" n) (expectCorrect <$> group)

    group0 :: [String]
    group0 =
      [ "/\\ a. /\\b. \\x : a. \\y : b. x : forall a. forall b. a -> b -> a"
      , "/\\ a. /\\b. \\x : a. \\y : b. x : forall a. forall b. a -> b -> a"
      , "/\\ a. \\x : a. /\\b. \\y : b. x : forall a. a -> forall b. b -> a"
      , "/\\ g. /\\ a. /\\ b. \\x : g -> a -> b. \\y : g -> a. \\z : g. x z (y z) : forall g. forall a. forall b. (g -> a -> b) -> (g -> a) -> g -> b"
      , "\\x : forall a.a.x[(forall a.a) -> (forall a.a) -> (forall a.a)] : (forall a. a) -> (forall a. a) -> (forall a. a) -> forall a. a"
      , "x : forall a.a |- x : forall a.a"
      , "x : forall a.a |- x[yes_honey] : yes_honey"
      ]

    group1 :: [String]
    group1 =
      [ "/\\Y. \\a : Y. \\c : Z. c: forall Y. Y -> Z -> Z"
      , "\\a : Y. /\\Z. \\b : Z. b: Y -> forall Z. Z -> Z"
      , "\\c : Y. \\a : Y. \\b : Y. b: Y -> Y -> Y -> Y"
      , "\\b : Y -> Y. \\a : Y. b a: (Y -> Y) -> Y -> Y"
      , "\\c : Y. (/\\Z. c)[Z]: Y -> Y"
      , "(/\\Y. \\c : Y. c)[Int]: Int -> Int"
      , "\\c : Y -> Y. \\b : Y. c b: (Y -> Y) -> Y -> Y"
      , "\\b : Y -> Z. \\a : Y. \\c : Y. a: (Y -> Z) -> Y -> Y -> Y"
      , "\\b : Y. /\\Z. /\\Z. b: Y -> forall Z. forall Z. Y"
      , "\\b : Z -> Y. \\c : Z. b c: (Z -> Y) -> Z -> Y"
      , "\\a : Z -> Z. \\b : Z. /\\Y. b: (Z -> Z) -> Z -> forall Y. Z"
      , "(/\\Y. \\b : Z. b)[Z]: Z -> Z"
      , "(/\\Z. \\b : Y. b)[Y -> Y]: Y -> Y"
      , "\\b : Y -> Y. /\\Z. \\c : Y. c: (Y -> Y) -> forall Z. Y -> Y"
      , "/\\Y. \\a : Y. \\b : Z. a: forall Y. Y -> Z -> Y"
      ]

    group2 :: [String]
    group2 =
      [ "\\b : X. /\\Y. \\a : Y. b: X -> forall Y. Y -> X"
      , "(/\\Z. \\a : Z. a)[Z -> X]: (Z -> X) -> Z -> X"
      , "/\\Y. /\\X. \\a : X. a: forall Y. forall X. X -> X"
      , "\\a : X. (/\\Z. a)[Z]: X -> X"
      , "\\c : X. /\\Z. \\a : X. c: X -> forall Z. X -> X"
      , "/\\X. \\a : Y. \\c : Z. a: forall X. Y -> Z -> Y"
      , "\\c : Y. /\\Z. \\a : Z. c: Y -> forall Z. Z -> Y"
      , "\\a : Y -> X. /\\Z. \\c : X. a: (Y -> X) -> forall Z. X -> Y -> X"
      , "\\c : Z -> X. \\a : Z. c a: (Z -> X) -> Z -> X"
      , "(/\\Z. \\c : X. c)[X -> X]: X -> X"
      , "\\a : Z -> Y. \\c : Z. a c: (Z -> Y) -> Z -> Y"
      , "/\\Z. \\b : X. /\\Y. b: forall Z. X -> forall Y. X"
      , "(/\\Z. \\a : Z. a)[Y -> Y]: (Y -> Y) -> Y -> Y"
      , "/\\X. \\c : X. \\b : Z. b: forall X. X -> Z -> Z"
      , "(/\\Y. \\a : Y. a)[Z -> X]: (Z -> X) -> Z -> X"
      , "(/\\X. \\b : Y. b)[X -> X]: Y -> Y"
      , "\\c : Y -> Z. \\a : Y. /\\X. a: (Y -> Z) -> Y -> forall X. Y"
      , "/\\X. \\b : X. /\\Y. b: forall X. X -> forall Y. X"
      , "/\\Y. /\\Y. \\a : Y. a: forall Y. forall Y. Y -> Y"
      , "\\a : Z -> Y. \\c : Z. a c: (Z -> Y) -> Z -> Y"
      ]

    group3 :: [String]
    group3 =
      [ "/\\V. \\c : (W -> Y) -> W. \\d : U -> X. \\b : Z. d: forall V. ((W -> Y) -> W) -> (U -> X) -> Z -> U -> X"
      , "/\\U. \\e : (Z -> Z) -> V -> Z. /\\W. \\b : W -> Y. e: forall U. ((Z -> Z) -> V -> Z) -> forall W. (W -> Y) -> (Z -> Z) -> V -> Z"
      , "\\e : U. \\d : Y. \\c : V -> X. /\\W. e: U -> Y -> (V -> X) -> forall W. U"
      , "\\c : Y. (/\\U. \\e : V -> V. e)[(U -> U) -> Y]: Y -> (V -> V) -> V -> V"
      , "/\\V. /\\Z. \\a : W -> Z. \\c : U -> X. a: forall V. forall Z. (W -> Z) -> (U -> X) -> W -> Z"
      , "\\d : Y -> Z. d: (Y -> Z) -> Y -> Z"
      ]

    group4 :: [String]
    group4 =
      [ "\\b : X. \\a : Z. \\c : X. c: X -> Z -> X -> X"
      , "\\c : X -> Y. \\b : X. \\a : X. c: (X -> Y) -> X -> X -> X -> Y"
      , "\\a : X -> Z. \\c : X. a c: (X -> Z) -> X -> Z"
      , "/\\Z. \\a : Z. \\c : Y. a: forall Z. Z -> Y -> Z"
      , "/\\X. \\c : Z. \\a : Z. a: forall X. Z -> Z -> Z"
      , "/\\X. /\\X. \\a : Y. a: forall X. forall X. Y -> Y"
      , "\\c : Y -> Z. \\b : Y. c b: (Y -> Z) -> Y -> Z"
      , "\\a : X. (/\\Z. a)[X]: X -> X"
      , "/\\Y. \\a : X. \\c : Z. a: forall Y. X -> Z -> X"
      , "/\\X. /\\X. \\c : Z. c: forall X. forall X. Z -> Z"
      , "\\a : Y. \\b : Z. \\c : X. b: Y -> Z -> X -> Z"
      , "/\\Z. /\\Y. \\b : X. b: forall Z. forall Y. X -> X"
      , "/\\X. \\b : X. \\c : Z. b: forall X. X -> Z -> X"
      , "\\b : Z -> Z. \\c : Z. b c: (Z -> Z) -> Z -> Z"
      , "\\b : Y -> X. \\c : Y. b c: (Y -> X) -> Y -> X"
      , "\\a : Z -> Z. /\\Y. /\\X. a: (Z -> Z) -> forall Y. forall X. Z -> Z"
      ]


incorrect :: TestTree
incorrect = testGroup "Incorrect typed expressions" $ expectIncorrect
  [ ("Free variable restriction fail", "x : a -> a |- /\\ a. \\y : a. y : forall a. a -> a")
  , ("self-application", "\\a : Y. \\c : Y. Y a: Y -> Y")
  , ("random rewrite"       , "\\a : Z -> Z. \\c : Z. a c: (Z -> Y) -> Z -> Y")
  , ("random rewrite"       , "/\\Z. \\b : X. /\\Y. b: forall X. X -> forall Y. X")
  , ("random rewrite"       , "(/\\Z. \\a : Z. a)[Z -> Y]: (Y -> Y) -> Y -> Y")
  , ("random rewrite"       , "/\\X. \\c : X. \\c : Z. b: forall X. X -> Z -> Z")
  , ("parenthesis"       , "(/\\Y. \\a : Y. a)[Z -> X]: ((Z -> X) -> Z) -> X")
  , ("random rewrite"       , "(/\\X. \\b : Y. b)[X -> X]: X -> Y")
  , ("random rewrite"       , "\\c : Y -> Z. \\a : Y. /\\X. x: (Y -> Z) -> Y -> forall X. Y")
  , ("random rewrite"       , "/\\X. \\b : X. /\\Y. b: forall X. X -> forall X. X")
  , ("random rewrite"       , "/\\Y. /\\Y. \\a : Y. a: forall Y. forall Y. a -> Y")
  , ("random rewrite"       , "\\a : Z -> Y. \\a : Z. a c: (Z -> Y) -> Z -> Y")
  ]

expectCorrect :: String -> TestTree
expectCorrect input = testCase input $ Right () @=? checkCorrect input

expectIncorrect :: [(String, String)] -> [TestTree]
expectIncorrect exprs = flip map exprs $
  \(msg, input) -> testCase msg $
    assertBool "Expecting Incorrect" (isLeft $ checkCorrect input)

checkCorrect :: String -> TErr ()
checkCorrect typedExpr = void parseAndTypeCheck
  where
    parseAndTypeCheck :: Either MyError Type
    parseAndTypeCheck  = do
      parsed <- parseWithPrettyError typedExpr
      case parsed of
        (ctx, expr, Just typ) -> typeCheck ctx (expr ::: typ)
        (ctx, expr, Nothing)  -> error "expected type presence"
