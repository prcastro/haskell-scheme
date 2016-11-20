module Lib (
    readExpr
) where

import Control.Monad
import Data.Complex
import Data.Ratio
import Numeric

import Text.ParserCombinators.Parsec hiding (spaces)


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Real Double
             | Rational Rational
             | Complex (Complex Double)
             | String String
             | Bool Bool
             | Character Char

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
            first <- letter <|> symbol
            rest <- many (letter <|> digit <|> symbol)
            let atom = first:rest
            return $ Atom atom

parseDecimal :: Parser LispVal
parseDecimal = do
               x <- many1 digit 
               (return . Number . read) x

parseDec :: Parser LispVal
parseDec = do
           try $ string "#d"
           x <- many1 digit 
           (return . Number . read) x

hex2dig x = fst . head $ readHex x

parseHex :: Parser LispVal
parseHex = do
           try $ string "#h"
           x <- many1 hexDigit 
           (return . Number . hex2dig) x

oct2dig x = fst . head $ readOct x

parseOct :: Parser LispVal
parseOct = do
           try $ string "#o"
           x <- many1 octDigit 
           (return . Number . oct2dig) x

bin2dig = bin2dig' 0
bin2dig' value "" = value
bin2dig' value (x:xs) = let newValue = 2 * value + (if x == '0' then 0 else 1) in
                        bin2dig' newValue xs

parseBin :: Parser LispVal
parseBin = do
           try $ string "#b"
           let binDigit = oneOf "10"
           x <- many1 binDigit
           (return . Number . bin2dig) x

parseNumber :: Parser LispVal
parseNumber = parseDecimal
          <|> parseDec
          <|> parseHex
          <|> parseOct
          <|> parseBin

parseReal :: Parser LispVal
parseReal = do
            x <- many1 digit
            char '.'
            y <- many1 digit
            (return . Real . fst . head . readFloat) (y ++ "." ++ x)

parseRational :: Parser LispVal
parseRational = do
                x <- many1 digit
                char '/'
                y <- many1 digit
                (return . Rational) ((read x) % (read y))

toDouble :: LispVal -> Double
toDouble (Real r) = realToFrac r
toDouble (Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do
               x <- try parseReal <|> parseDecimal
               char '+'
               y <- try parseReal <|> parseDecimal
               char 'i'
               (return . Complex) (toDouble x :+ toDouble y)

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
              x <- many $ try escapedChars <|> noneOf "\""
              char '"'
              return $ String x

parseBool :: Parser LispVal
parseBool = do
            char '#'
            val <- oneOf "ft"
            return $ case val of
                'f' -> Bool False
                't' -> Bool True

charToString :: Char -> String
charToString x = [x]

parseCharacter :: Parser LispVal
parseCharacter = do
                 string "#\\"
                 value <- try (string "newline" <|> string "space")
                      <|> do
                          x <- anyChar
                          notFollowedBy alphaNum
                          return $ charToString x
                 return $ Character $ case value of
                     "space" -> ' '
                     "newline" -> '\n'
                     otherwise -> value !! 0

parseExpr :: Parser LispVal
parseExpr = try parseAtom
        <|> try parseReal
        <|> try parseRational
        <|> try parseComplex
        <|> try parseNumber
        <|> try parseString
        <|> try parseBool
        <|> parseCharacter

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> case val of
        List x -> "Found List value"
        DottedList xs x -> "Found DottedList value"
        Real x -> "Found Real value"
        Rational x -> "Found Rational value"
        Complex x -> "Found Complex value"
        Number x -> "Found Number value " ++ show x
        String x -> "Found String value"
        Bool x -> "Found Bool value"
        Character x -> "Found Character value"
