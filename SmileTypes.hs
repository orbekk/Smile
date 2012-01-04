module SmileTypes where

data AeExpr = Number Int
            | Add AeExpr AeExpr
            | Id String
            | With AeExpr AeExpr AeExpr
            | Function AeExpr AeExpr
            | App AeExpr AeExpr
    deriving (Show, Eq)
