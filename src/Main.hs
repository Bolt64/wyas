module Main where

import DataTypes
import SourceParser
import Evaluator
import System.Environment
import Control.Monad (liftM)

main :: IO ()
main = do
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
        putStrLn $ extractValue $ trapError evaled

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
