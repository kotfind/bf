module Exec (exec) where

----------------------------------------------------------------------

import Token
import Data.Char (ord, chr)

----------------------------------------------------------------------

data Tape = Tape [Int] [Int] -- pointer shows on head of right list

instance Show Tape where
    show (Tape l r) = "..., " ++ fmt left ++ ", [" ++ show mid ++ "]" ++ fmt right ++ ", ..."
      where
        fmt arr = foldl (\acc el -> acc ++ ", " ++ show el) "" arr

        left    = (reverse . take 5) l
        mid     = head r
        right   = take 5 $ tail r

----------------------------------------------------------------------

apply :: Tape -> Token -> IO Tape
apply t@(Tape l r) (Op c n) = case c of
    '+' -> return $ Tape l (head r + n : tail r)
    '-' -> return $ Tape l (head r - n : tail r)
    '*' -> return $ Tape l (n : tail r)
    '<' -> return $ Tape (drop n l) (reverse (take n l) ++ r)
    '>' -> return $ Tape (reverse (take n r) ++ l) (drop n r)
    '.' -> (putChar . chr . head) r >> return t
    ',' -> getChar >>= \c -> return $ Tape l (ord c : tail r)

exec' :: Bool -> TokenExpr -> Tape -> IO Tape
exec' debug (TokenExpr []) t = do
    putStr $ if debug 
        then "\n\n\t" ++ show t ++ "\n\tNone\n"
        else ""
    return t
exec' debug expr@(TokenExpr (tok : toks)) t@(Tape l r) = do
    putStr $ if debug
        then "\n\n\t" ++ show t ++ "\n\t" ++ show expr ++ "\n"
        else ""
    case tok of
        Op _ _      -> apply t tok >>= exec' debug (TokenExpr toks)
        Cycle cyc   -> if head r == 0
            then exec' debug (TokenExpr toks) t
            else exec' debug cyc t >>= exec' debug expr

----------------------------------------------------------------------

exec :: Bool -> String -> IO ()
exec debug code = do
    let prog = read code :: TokenExpr
    let tape = Tape (repeat 0) (repeat 0)
    exec' debug prog tape
    return ()
