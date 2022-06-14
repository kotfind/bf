module Token
    ( Token (..)
    , TokenExpr (..)
    ) where

----------------------------------------------------------------------

import Text.ParserCombinators.ReadP

----------------------------------------------------------------------

data Token
    = Op Char Int
    | Cycle TokenExpr

instance Show Token where
    show (Op c n) = replicate n c
    show (Cycle ts) = "[" ++ show ts ++ "]"

instance Read Token where
    readsPrec _ = readP_to_S parseToken

parseToken :: ReadP Token
parseToken = parseOp +++ parseCycle

parseOp :: ReadP Token
parseOp = foldl1 (+++) [do
    n <- munch1 (== c)
    return $ Op c (length n)
    | c <- "+-<>.,"]

parseCycle :: ReadP Token
parseCycle = do
    char '['
    n <- parseTokenExpr
    char ']'
    return $ Cycle n

----------------------------------------------------------------------

data TokenExpr = TokenExpr [Token]

instance Show TokenExpr where
    show (TokenExpr ts) = foldl (\acc t -> acc ++ show t) "" ts

instance Read TokenExpr where
    readsPrec _ = readP_to_S parseTokenExpr

parseTokenExpr :: ReadP TokenExpr
parseTokenExpr = do
    n <- many $ parseToken
    return $ TokenExpr n
