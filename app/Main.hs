module Main where

import Game
import Lib

main :: IO ()
main = do 
    { putStrLn "Board:"
    ; putStrLn $ show makeNewGame ++ "\n"
    ; putStrLn "Make move: " 
    ; inStr <- getLine
    ; putStrLn inStr
    }
