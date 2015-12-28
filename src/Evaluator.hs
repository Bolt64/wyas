module Evaluator
    (
    eval
    ) where

import DataTypes
import PrimitiveFunctions
import StateManager

-- Eval function

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(LispString _) = return val
eval env val@(Number _) = return val
eval env val@(Gaussian _) = return val
eval env val@(LispBool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List (Atom "if" : listArgs)) = lispIf env listArgs
eval env (List [Atom "set!", Atom var, form]) = lispSetVar env var form
eval env (List [Atom "define", Atom var, form]) = lispDefineVar env var form
eval env (List (Atom "cond" : listArgs)) = lispCond env listArgs
eval env (List (Atom "case" : (key : clauses))) = lispCase env key clauses
eval env (List (Atom func: args)) = mapM (eval env) args >>= apply func env
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- evaluate a sequence of lispVals in order

{-evalSeq :: Env -> [LispVal] -> IOThrowsError LispVal-}
{-evalSeq -}

-- New type alias for convenience

type LispFunction = Env -> [LispVal] -> IOThrowsError LispVal

-- Code to implement if

defIf :: LispFunction
defIf env [pred, conseq, alt] = do result <- eval env pred
                                   case result of
                                        LispBool True -> eval env conseq
                                        LispBool False -> eval env alt
                                        badArg -> throwError $ TypeMismatch "LispBool" badArg

lispIf :: LispFunction
lispIf env listArgs = defIf env =<< (countArgs 3 listArgs)

-- A whole lot of code to implement cond

defEvalClause :: LispFunction
defEvalClause env [(List [Atom "else", expression])] = defEvalClause env [(List [LispBool True, expression])]
defEvalClause env [(List [test, expression])] = do result <- eval env test
                                                   case result of
                                                    LispBool True -> do evaled <- eval env expression
                                                                        return $ List [LispBool True, evaled]
                                                    LispBool False -> return $ List [LispBool False]
                                                    badEval -> throwError $ TypeMismatch "LispBool" badEval
defEvalClause env [badArg] = throwError $ TypeMismatch "clause" badArg

evalClause :: LispFunction
evalClause env listArgs = defEvalClause env =<< (countArgs 1 listArgs)

lispCond :: LispFunction
lispCond env [] = throwError $ BadSpecialForm "No clause evaluated to true" (List [])
lispCond env (x:xs) = do result <- evalClause env [x]
                         case result of
                            List [LispBool True, output] -> return output
                            List [LispBool False] -> lispCond env xs

-- Code to implement case

evalCaseClause :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
evalCaseClause env _ (List [Atom "else", expression]) = do result <- eval env expression
                                                           return $ List [LispBool True, result]
evalCaseClause env key (List [datum, expression]) = do evaled <- eval env key
                                                       ifContains <- listContains env [evaled, datum]
                                                       case ifContains of
                                                        LispBool True -> do result <- eval env expression
                                                                            return $ List [LispBool True, result]
                                                        LispBool False -> return $ List [LispBool False]

lispCase :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
lispCase _ _ [] = throwError $ BadSpecialForm "Non exhaustive pattern match" (List [])
lispCase env key (clause:rest) = do result <- evalCaseClause env key clause
                                    case result of
                                        List [LispBool True, output] -> return output
                                        List [LispBool False] -> lispCase env key rest

-- Code for setVar and defineVar

lispSetVar :: Env -> String -> LispVal -> IOThrowsError LispVal
lispSetVar env var form = eval env form >>= setVar env var

lispDefineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
lispDefineVar env var form = eval env form >>= defineVar env var

-- function application

apply :: String -> LispFunction
apply func env args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) (applyTwo env args) (lookup func primitives)

-- Helper function
applyTwo :: a -> b -> (a -> b -> c) -> c
applyTwo a b func = func a b
