module Parser where
import Text.ParserCombinators.Parsec (
    Parser, many, many1, parse, (<|>), char, skipMany
    )
import Text.ParserCombinators.Parsec.Char (
    digit, alphaNum, letter, string, space, newline
    )
import SmileTypes

pExpr :: Parser AeExpr
pExpr = pBracketed pExpr_ <|> pId <|> pNumber

-- Expression in brackets.
pExpr_ :: Parser AeExpr
pExpr_ = pAdd <|> pWith <|> pFunction <|> pApplication <|> pExpr

pWith :: Parser AeExpr
pWith = do
    string "with"
    pWhitespace
    (id, boundExpr) <- pBracketed pBinding
    pWhitespace
    inExpr <- pExpr
    return (With id boundExpr inExpr)

pFunction :: Parser AeExpr
pFunction = do
    string "fun"
    pWhitespace
    paramId <- pBracketed pId
    pWhitespace
    bodyExpr <- pExpr
    return (Function paramId bodyExpr)

pApplication :: Parser AeExpr
pApplication = do
    function <- pExpr
    pWhitespace
    param <- pExpr
    return (App function param)

pBinding :: Parser (AeExpr, AeExpr)
pBinding = do
    id <- pId
    pWhitespace
    boundExpr <- pExpr
    return (id, boundExpr)

pBracketed :: Parser a -> Parser a
pBracketed parser = do
    char '('
    pWhitespace
    expr <- parser
    pWhitespace
    char ')'
    return expr

pAdd :: Parser AeExpr
pAdd = do
    char '+'
    pWhitespace
    arg1 <- pExpr
    pWhitespace
    arg2 <- pExpr
    return (Add arg1 arg2)

pNumber :: Parser AeExpr
pNumber = do
    digits <- many1 digit
    return (Number (read digits))

pId :: Parser AeExpr
pId = do
    firstChar <- letter
    rest <- many alphaNum
    return (Id (firstChar : rest))

pWhitespace :: Parser ()
pWhitespace = skipMany (space <|> newline)

parseOrDie :: String -> AeExpr
parseOrDie input = case parse pExpr "input" input of
    Right val -> val
    Left err -> error ("Failed to parse: " ++ show err)

