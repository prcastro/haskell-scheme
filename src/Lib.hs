module Lib (
    readExpr
) where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do
               char '\\'
               x <- oneOf "\\\"nrt"
               return $ case x of
                   '\\' -> x
                   '"' -> x
                   'n' -> '\n'
                   'r' -> '\r'
                   't' -> '\t'

parseString :: Parser LispVal
parseString = do
              char '"'
              x <- many $ escapedChars <|> noneOf "\""
              char '"'
              return $ String x

parseAtom :: Parser LispVal
parseAtom = do
            first <- letter <|> symbol
            rest <- many (letter <|> digit <|> symbol)
            let atom = first:rest
            return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= (return . Number . read)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
