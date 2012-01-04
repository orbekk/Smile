module SmileTypes where

data AeExpr = Number Int
            | Add AeExpr AeExpr
    deriving (Show, Eq)
