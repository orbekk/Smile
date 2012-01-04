module Main where

import Parser
import Eval

main = do
    contents <- getContents
    putStrLn (show (eval (parseOrDie contents)))
