module Main where
import Parser

main = do
    contents <- getContents
    putStrLn (show (parseOrDie contents))
