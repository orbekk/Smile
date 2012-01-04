module Parser where
import Text.ParserCombinators.Parsec (
    Parser, many1, parse, (<|>), char, spaces
    )
import Text.ParserCombinators.Parsec.Char (digit)
import SmileTypes

pExpr :: Parser AeExpr
pExpr = pAdd <|> pNumber

pAdd :: Parser AeExpr
pAdd = do
    char '+'
    spaces
    arg1 <- pExpr
    spaces
    arg2 <- pExpr
    return (Add arg1 arg2)

pNumber :: Parser AeExpr
pNumber = do
    digits <- many1 digit
    return (Number (read digits))

parseOrDie :: String -> AeExpr
parseOrDie input = case parse pExpr "input" input of
    Right val -> val
    Left err -> error ("Failed to parse: " ++ show err)
