module Evaluator
    (
    eval
    ) where

import DataTypes
import PrimitiveFunctions

-- Eval function

eval :: LispVal -> ThrowsError LispVal
eval val@(LispString _) = return val
eval val@(Number _) = return val
eval val@(Gaussian _) = return val
eval val@(LispBool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom "if" : listArgs)) = lispIf listArgs
eval (List (Atom "cond" : listArgs)) = lispCond listArgs
eval (List (Atom "case" : (key : clauses))) = lispCase key clauses
eval (List (Atom func: args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- New type alias for convenience

type LispFunction = [LispVal] -> ThrowsError LispVal

-- Code to implement if

defIf :: LispFunction
defIf [pred, conseq, alt] = do result <- eval pred
                               case result of
                                    LispBool True -> eval conseq
                                    LispBool False -> eval alt
                                    badArg -> throwError $ TypeMismatch "LispBool" badArg

lispIf :: LispFunction
lispIf listArgs = defIf =<< (countArgs 3 listArgs)

-- A whole lot of code to implement cond

defEvalClause :: LispFunction
defEvalClause [(List [Atom "else", expression])] = defEvalClause [(List [LispBool True, expression])]
defEvalClause [(List [test, expression])] = do result <- eval test
                                               case result of
                                                LispBool True -> do evaled <- eval expression
                                                                    return $ List [LispBool True, evaled]
                                                LispBool False -> return $ List [LispBool False]
                                                badEval -> throwError $ TypeMismatch "LispBool" badEval
defEvalClause [badArg] = throwError $ TypeMismatch "clause" badArg

evalClause :: LispFunction
evalClause listArgs = defEvalClause =<< (countArgs 1 listArgs)

lispCond :: LispFunction
lispCond [] = throwError $ BadSpecialForm "No clause evaluated to true" (List [])
lispCond (x:xs) = do result <- evalClause [x]
                     case result of
                        List [LispBool True, output] -> return output
                        List [LispBool False] -> lispCond xs

-- Code to implement case

evalCaseClause :: LispVal -> LispVal -> ThrowsError LispVal
evalCaseClause _ (List [Atom "else", expression]) = do result <- eval expression
                                                       return $ List [LispBool True, result]
evalCaseClause key (List [datum, expression]) = do evaled <- eval key
                                                   ifContains <- listContains [evaled, datum]
                                                   case ifContains of
                                                        LispBool True -> do result <- eval expression
                                                                            return $ List [LispBool True, result]
                                                        LispBool False -> return $ List [LispBool False]

lispCase :: LispVal -> LispFunction
lispCase _ [] = throwError $ BadSpecialForm "Non exhaustive pattern match" (List [])
lispCase key (clause:rest) = do result <- evalCaseClause key clause
                                case result of
                                    List [LispBool True, output] -> return output
                                    List [LispBool False] -> lispCase key rest

-- function application

apply :: String -> LispFunction
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) (lookup func primitives)
