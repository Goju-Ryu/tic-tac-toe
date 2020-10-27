module Main where

import Game
import Lib


userIO :: GameState -> IO String
userIO s = do 
    { putStrLn "Board:"
    ; putStrLn $ show s ++ "\n"
    ; putStrLn "Make move: " 
    ; getLine  -- This value is returned
    }

readMove :: String -> Move
readMove str = read str

gameLoop :: GameState -> Player -> IO b
gameLoop s p = do 
    { inStr <- userIO s
    ; let move = readMove inStr
    ; let result = makeMove p move s
    ; case (result) of 
    ;    Left newState -> gameLoop  newState $ swichPlayer p
    ;    Right (msg, oldState) -> putStrLn msg >> gameLoop oldState p
    } 

main :: IO ()
main = gameLoop makeNewGame PlayerX
