module Lib (
    readExpr
  , eval
  , valueToPrint
) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Error.Class
import Data.Array
import Data.Complex
import Data.Ratio
import Numeric

import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | Vector (Array Int LispVal)
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Real Double
             | Rational Rational
             | Complex (Complex Double)
             | String String
             | Character Char
             | Bool Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

showVals :: [LispVal] -> String
showVals = unwords . map show

instance Show LispVal where
    show (Atom name) = name
    show (Vector contents) = "(" ++ (showVals $ elems contents) ++ ")"
    show (List contents) = "(" ++ showVals contents ++ ")"
    show (DottedList init last) = "(" ++ showVals init ++ " . " ++ show last ++ ")"
    show (Number n) = show n
    show (Real x) = show x
    show (Rational r) = show r
    show (Complex c) = show c
    show (String content) = content
    show (Character c) = charToString c
    show (Bool True) = "#t"
    show (Bool False) = "#f"

instance Show LispError where
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ show func
    show (NumArgs expected found) = "Expected " ++ show expected ++ " arguments. Found values " ++ showVals found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

spaces :: Parser ()
spaces = skipMany1 space

parseVector' :: Parser LispVal
parseVector' = do
    vectorValues <- sepBy parseExpr spaces
    return $ Vector (listArray (0, (length vectorValues - 1)) vectorValues)

parseVector :: Parser LispVal
parseVector = do
    string "#("
    x <- parseVector'
    char ')'
    return x

parseDatum :: Parser LispVal
parseDatum = do
    char '.'
    spaces
    parseExpr

parseListContent :: Parser LispVal
parseListContent = do
    list <- sepEndBy parseExpr spaces
    datum <- optionMaybe parseDatum
    return $ case datum of
        Nothing -> List list
        Just datum -> DottedList list datum

parseList :: Parser LispVal
parseList = do
    char '('
    list <- parseListContent
    char ')'
    return list

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

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
        '"'  -> x
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ try escapedChars <|> noneOf "\""
    char '"'
    return $ String x

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

parseBool :: Parser LispVal
parseBool = do
    char '#'
    val <- oneOf "ft"
    return $ case val of
        'f' -> Bool False
        't' -> Bool True

parseExpr :: Parser LispVal
parseExpr = try parseAtom
        <|> try parseVector
        <|> try parseList
        <|> try parseQuoted
        <|> try parseQuasiQuoted
        <|> try parseUnquoted
        <|> try parseReal
        <|> try parseRational
        <|> try parseComplex
        <|> try parseNumber
        <|> try parseString
        <|> try parseCharacter
        <|> try parseBool


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numberOp (+)),
              ("-", numberOp (-)),
              ("*", numberOp (*)),
              ("/", numberOp div),
              ("mod", numberOp mod),
              ("quotient", numberOp quot),
              ("remainder", numberOp rem),
              ("=", numberOpBool (==)),
              ("<", numberOpBool (<)),
              (">", numberOpBool (>)),
              ("/=", numberOpBool (/=)),
              ("<=", numberOpBool (<=)),
              (">=", numberOpBool (>=)),
              ("&&", boolOpBool (&&)),
              ("||", boolOpBool (||)),
              ("string=?", stringOpBool (==)),
              ("string<?", stringOpBool (<)),
              ("string>?", stringOpBool (>)),
              ("string<=?", stringOpBool (<=)),
              ("string>=?", stringOpBool (>=)),
              ("symbol?", isAtom),
              ("vector?", isVector),
              ("list?", isList),
              ("number?", isNumber),
              ("real?", isReal),
              ("rational?", isRational),
              ("complex?", isComplex),
              ("string?", isString),
              ("character?", isCharacter),
              ("bool?", isBool),
              ("string->symbol", string2symbol),
              ("symbol->string", symbol2string)]

unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber notNumber = throwError $ TypeMismatch "number" notNumber

unpackString :: LispVal -> ThrowsError String
unpackString (String n) = return n
unpackString notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool n) = return n
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

getNumberValues :: [LispVal] -> ThrowsError [Integer]
getNumberValues lispvals = mapM unpackNumber lispvals

numberOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numberOp op [] = throwError $ NumArgs 2 []
numberOp op arg@[x] = throwError $ NumArgs 2 arg
numberOp op args = getNumberValues args >>= return . Number . foldl1 op

opBool :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
opBool unpacker op args = if length args /= 2
                          then throwError $ NumArgs 2 args
                          else do left <- unpacker $ head args
                                  right <- unpacker $ last args
                                  return $ Bool $ left `op` right

numberOpBool = opBool unpackNumber
stringOpBool = opBool unpackString
boolOpBool = opBool unpackBool

isAtom :: [LispVal] -> ThrowsError LispVal
isAtom [Atom _] = return $ Bool True
isAtom [List (Atom "quoted":xs)] = return $ Bool True
isAtom [_] = return $ Bool False
isAtom x = throwError $ NumArgs 1 x

isVector :: [LispVal] -> ThrowsError LispVal
isVector [Vector _] = return $ Bool True
isVector [_] =return $  Bool False
isVector x = throwError $ NumArgs 1 x

isList :: [LispVal] -> ThrowsError LispVal
isList [List _] = return $ Bool True
isList [DottedList _ _] = return $ Bool True
isList [_] = return $ Bool False
isList x = throwError $ NumArgs 1 x

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber [_] = return $ Bool False
isNumber x = throwError $ NumArgs 1 x

isReal :: [LispVal] -> ThrowsError LispVal
isReal [Number _] = return $ Bool True
isReal [_] = return $ Bool False
isReal x = throwError $ NumArgs 1 x

isRational :: [LispVal] -> ThrowsError LispVal
isRational [Number _] = return $ Bool True
isRational [_] = return $ Bool False
isRational x = throwError $ NumArgs 1 x

isComplex :: [LispVal] -> ThrowsError LispVal
isComplex [Number _] = return $ Bool True
isComplex [_] = return $ Bool False
isComplex x = throwError $ NumArgs 1 x

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString [_] = return $ Bool False
isString x = throwError $ NumArgs 1 x

isCharacter :: [LispVal] -> ThrowsError LispVal
isCharacter [Character _] = return $ Bool True
isCharacter [_] = return $ Bool False
isCharacter x = throwError $ NumArgs 1 x

isBool :: [LispVal] -> ThrowsError LispVal
isBool [Bool _] = return $ Bool True
isBool [_] = return $ Bool False
isBool x = throwError $ NumArgs 1 x

string2symbol :: [LispVal] -> ThrowsError LispVal
string2symbol [String s] = return $ Atom s
string2symbol [x] = throwError $ TypeMismatch "string" x
string2symbol x = throwError $ NumArgs 1 x

symbol2string :: [LispVal] -> ThrowsError LispVal
symbol2string [Atom s] = return $ String s
symbol2string [x] = throwError $ TypeMismatch "symbol" x
symbol2string x = throwError $ NumArgs 1 x

valueToPrint = extractValue . trapError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval atom@(Atom _) = return atom
eval vector@(Vector _) = return vector
eval list@(List [Atom "quote", value]) = return value
eval list@(List (Atom func : args)) = mapM eval args >>= apply func
eval list@(List _) = return list
eval dotted@(DottedList _ _) = return dotted
eval number@(Number _) = return number
eval real@(Real _) = return real
eval rational@(Rational _) = return rational
eval complex@(Complex _) = return complex
eval string@(String _) = return string
eval char@(Character _) = return char
eval bool@(Bool _) = return bool

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right value -> return value
