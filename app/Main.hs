module Main where

import System.Environment 
import Lib

main :: IO ()
main = do
    putStrLn "State your name"
    name <- getLine
    putStrLn $ "Welcome, " ++ name
    args <- getArgs
    let x = read (args !! 0) :: Int
        y = read (args !! 1) :: Int
    putStrLn $ "The result of the sum is: " ++ (show (x + y))
