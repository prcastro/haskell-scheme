module Main where

import Control.Monad
import System.Environment

import Lib

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (head args) >>= eval
    putStrLn $ valueToPrint evaled

