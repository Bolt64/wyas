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
eval env (List [Atom "set!", Atom var, form]) = 
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "cond" : listArgs)) = lispCond env listArgs
eval env (List (Atom "case" : (key : clauses))) = lispCase env key clauses
eval env (List (Atom func: args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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
lispIf env listArgs = defIf =<< (countArgs 3 listArgs)

-- A whole lot of code to implement cond

defEvalClause :: LispFunction
defEvalClause env [(List [Atom "else", expression])] = defEvalClause [(List [LispBool True, expression])]
defEvalClause env [(List [test, expression])] = do result <- eval env test
                                                   case result of
                                                    LispBool True -> do evaled <- eval env expression
                                                                     return $ List [LispBool True, evaled]
                                                    LispBool False -> return $ List [LispBool False]
                                                    badEval -> throwError $ TypeMismatch "LispBool" badEval
defEvalClause env [badArg] = throwError $ TypeMismatch "clause" badArg

evalClause :: LispFunction
evalClause env listArgs = defEvalClause =<< (countArgs 1 listArgs)

lispCond :: LispFunction
lispCond env [] = throwError $ BadSpecialForm "No clause evaluated to true" (List [])
lispCond env (x:xs) = do result <- evalClause env [x]
                         case result of
                            List [LispBool True, output] -> return output
                            List [LispBool False] -> lispCond env xs

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
