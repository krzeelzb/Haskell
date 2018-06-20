import System.IO
import Data.Char
---------------------------------------
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hello " ++ name ++ " good to see you!"
    
---------------------------------------
guess = do
       putStrLn $ "Guess a number "
       loop 0
   
num = 5
loop :: Int  -> IO ()
loop 3=return()
loop guesses = do
    userInput <- readLn
    case compare num userInput of
        EQ -> putStrLn "Correct!" >> main
        LT -> putStrLn "Too big" >> loop (guesses+1)
        GT -> putStrLn "Too small" >> loop (guesses+1)
---------------------------------------
reverse' = do
    name <- getLine
    putStrLn $ reverse name 
---------------------------------------
upper = do     
    contents <- readFile "machine.txt"     
    writeFile "new_machine.txt" (map toUpper contents) 

