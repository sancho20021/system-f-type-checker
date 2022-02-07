module Main
  ( main
  ) where
import Base (MyError (ParseError), TypedExpr ((:::)))
import Checker (inferType, typeCheck)
import Parser (parseWithPrettyError)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let showErr = case args of
        "show" : _ -> show
        _          -> shortErr
  result <- handleInput <$> getLine
  let output = case result of
        Left me -> showErr me
        Right s -> s
  putStrLn output
  where
    shortErr :: MyError -> String
    shortErr (ParseError _) = "No parse"
    shortErr _              = "Incorrect"

    handleInput :: String -> Either MyError String
    handleInput input = do
      (ctx, expr, typ) <- parseWithPrettyError input
      case typ of
        Nothing -> show <$> inferType ctx expr
        Just ty -> "Correct" <$ typeCheck ctx (expr ::: ty)
