module Eval where

import SmileTypes

eval :: AeExpr -> AeExpr
eval (Id x) = error ("Unknown identifier: " ++ x)
eval (Number n) = Number n
eval (Add expr1 expr2) = Number (val1 + val2)
    where (Number val1) = eval expr1
          (Number val2) = eval expr2
eval (With id boundExpr inExpr) = eval (substitute id boundExpr inExpr)
eval expr@(Function id bodyExpr) = expr
eval (App fun paramExpr) =
    case fun of
        Function id bodyExpr -> eval (substitute id paramExpr bodyExpr)
        _ -> error ("Function application with non-function " ++ show fun)

substitute :: AeExpr -> AeExpr -> AeExpr -> AeExpr
substitute (Id x) boundExpr inExpr = substitute_ inExpr
    where
    substitute_ (Number x) = Number x
    substitute_ (Add expr1 expr2) = Add (substitute_ expr1) (substitute_ expr2)
    substitute_ (Id y) = if x == y then boundExpr else Id y
    substitute_ (With (Id y) boundExpr0 inExpr0) =
        if x == y
            then With (Id y) (substitute_ boundExpr0) inExpr0
            else With (Id y) (substitute_ boundExpr0) (substitute_ inExpr0)
    substitute_ (Function (Id y) bodyExpr) =
        if x == y
            then Function (Id y) bodyExpr
            else Function (Id y) (substitute_ bodyExpr)
    substitute_ (App funExpr paramExpr) =
        App (substitute_ funExpr) (substitute_ paramExpr)
