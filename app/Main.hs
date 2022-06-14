module Main where

import Exec (exec)
import System.IO (readFile)
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
    cmd     <- getProgName
    args    <- getArgs

    let is_debug_flag = (`elem` ["--debug", "-d"])
    let debug = any is_debug_flag args
    let args' = filter (not . is_debug_flag) args

    if length args' /= 1
    then putStrLn $ "Usage: " ++ cmd ++ " [-d | --debug] filename"
    else do
        let filename:[] = args'
        code <- readFile filename
        exec debug code
