module Parser
  ( ParseResult
  , parse
  , parseWithPrettyError
  ) where
import Base (Context, EVar, Expr (ELam, EVar, TLam, (:$), (:@)), MyError (ParseError), TVar,
             Type (Forall, TVar, (:->)))
import Control.Applicative (Alternative (many, some), optional, (<|>))
import Control.Monad (void)
import Data.Char (isAlphaNum)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, label, lookAhead, notFollowedBy, try), ParseErrorBundle,
                        Parsec, between, errorBundlePretty, runParser, satisfy, sepBy)
import Text.Megaparsec.Char (char, letterChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- Grammar:
-- input            ::= [context '|-'] expr [':' type]

-- context          ::= eps | variable ':' type [',' context]

-- type             ::= variable
--                    | (type)
--                    | (type) '->' type
--                    | variable '->' type
--                    | forall variable. type

-- expr             ::= [application] '\' variable ':' type '.' expr
--                    | [application] '/\' variable '.' expr
--                    | application

-- application      ::= type_application
--                    | application type_application

-- type_application ::= atom
--                    | type_application '[' type ']'

-- atom             ::= (expr)
--                    | variable

-- variable         ::= [a-z] [a-z0-9'_]*

type Parser = Parsec Void String

type ParseResult = (Context, Expr, Maybe Type)

parseWithPrettyError :: String -> Either MyError ParseResult
parseWithPrettyError input = case parse input of
  Left err -> Left $ ParseError $ errorBundlePretty err
  Right x0 -> Right x0

parse
  :: String
  -> Either (ParseErrorBundle String Void) ParseResult
parse = runParser pInput "Input"

pInput :: Parser ParseResult
pInput = between space eof pInput'
  where
    pInput' :: Parser ParseResult
    pInput' = do
      context <- optional (try pContext <* symbol "|-")
      expr <- pExpr
      typ <- optional (symbol ":" *> pType)
      return (fromMaybe Map.empty context, expr, typ)

pContext :: Parser Context
pContext = Map.fromList <$> pTypedVar `sepBy` symbol ","
  where
    pTypedVar :: Parser (EVar, Type)
    pTypedVar = do
      var <- pEVar
      void $ symbol ":"
      typ <- pType
      return (var, typ)

pExpr :: Parser Expr
pExpr = pApply'

pTVar :: Parser TVar
pTVar = pVarCommon

pEVar :: Parser EVar
pEVar = pVarCommon

keywords :: [String]
keywords = ["forall"]

pVarCommon :: Parser String
pVarCommon = label "variable" $ do
  r <- lookAhead pId
  if r `elem` keywords
  then fail "unexpected keyword"
  else pId

pId :: Parser String
pId = lexeme ((:) <$> letterChar <*> pIdTail)
  where
    pIdTail :: Parser String
    pIdTail = many pIdPart

pIdPart :: Parser Char
pIdPart = satisfy isAlphaNum <|> char '\'' <|> char '_'

pApply :: Parser Expr
pApply = foldl1 (:$) <$> some pTypeApply

pApply' :: Parser Expr
pApply' = pELam <|> pTLam <|> do
  h <- pApply
  lam <- optional (pELam <|> pTLam)
  return $ case lam of
    Nothing -> h
    Just l  -> h :$ l

pTypeApply :: Parser Expr
pTypeApply = do
  first <- pAtom
  rest <- many pTypeInBrackets
  return $ foldl (:@) first  rest
  where
    pTypeInBrackets :: Parser Type
    pTypeInBrackets = inParen "[" "]" pType

pELam :: Parser Expr
pELam = label "lambda abstraction" $ do
  void $ symbol "\\"
  x <- pEVar
  void $ symbol ":"
  typ <- pType
  void $ symbol "."
  ELam x typ <$> pExpr

pTLam :: Parser Expr
pTLam = label "type abstraction" $ do
  void $ symbol "/\\"
  a <- pTVar
  void $ symbol "."
  TLam a <$> pExpr

pAtom :: Parser Expr
pAtom = inParen "(" ")" pExpr <|> (EVar <$> pEVar)

-- Types -----------------------------------------------
pType :: Parser Type
pType = do
  lp <- pIncompleteT <|> (Right <$> pForall)
  case lp of
    Right complete -> return complete
    Left incomplete -> do
      rp <- optional $ symbol "->"  >> pType
      return $ case rp of
        Nothing  -> incomplete
        Just rp' -> incomplete :-> rp'
  where
    pForall :: Parser Type
    pForall = do
      void $ pKeyword "forall"
      v <- pTVar
      void $ symbol "."
      Forall v <$> pType

    pIncompleteT :: Parser (Either Type Type)
    pIncompleteT = Left <$> (inParen "(" ")" pType <|> (TVar <$> pTVar))

-- Help functions
pKeyword :: String -> Parser String
pKeyword s = lexeme (string s <* notFollowedBy pIdPart)

inParen :: String -> String -> Parser a -> Parser a
inParen l r = symbol l `between` symbol r

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space
