module Eval where

import SmileTypes

eval :: AeExpr -> AeExpr
eval (Number n) = Number n
eval (Add expr1 expr2) = Number (val1 + val2)
    where (Number val1) = eval expr1
          (Number val2) = eval expr2
