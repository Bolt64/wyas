module PrimitiveFunctions
    (
    primitives,
    countArgs,
    listContains
    ) where

import DataTypes
import StateManager

type LispFunction = Env -> [LispVal] -> IOThrowsError LispVal

primitives :: [(String, LispFunction)]
primitives = [
                ("+", numericBinop (+)),
                ("-", numericBinop (-)),
                ("*", numericBinop (*)),
                ("/", numericBinop div),
                ("mod", numericBinop mod),
                ("quot", numericBinop quot),
                ("rem", numericBinop rem),
                ("string?", isLispString),
                ("number?", isNumber),
                ("class-of", getType),
                ("=", numBoolBinop (==)),
                ("/=", numBoolBinop (/=)),
                ("<", numBoolBinop (<)),
                ("<=", numBoolBinop (<=)),
                (">", numBoolBinop (>)),
                (">=", numBoolBinop (>=)),
                ("&&", boolBoolBinop (&&)),
                ("||", boolBoolBinop (||)),
                ("string=?", strBoolBinop (==)),
                ("string/=?", strBoolBinop (/=)),
                ("string>?", strBoolBinop (>)),
                ("string<?", strBoolBinop (<)),
                ("string>=?", strBoolBinop (>=)),
                ("string<=?", strBoolBinop (<=)),
                ("car", car),
                ("cdr", cdr),
                ("cons", cons),
                ("eqv?", eqv),
                ("eq?", eqv)
             ]

numericBinop :: (Integer -> Integer -> Integer) -> (LispFunction)
numericBinop op _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op _ params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> IOThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

isLispString :: LispFunction
isLispString _ ([LispString _]) = return $ LispBool True
isLispString _ ([_]) = return $ LispBool False
isLispString _ (multiArgs@(_:_)) = throwError $ NumArgs 1 multiArgs

isNumber :: LispFunction
isNumber _ ([Number _]) = return $ LispBool True
isNumber _ ([_]) = return $ LispBool False
isNumber _ (multiArgs@(_:_)) = throwError $ NumArgs 1 multiArgs

getType :: LispFunction
getType _ [x] = return $ Atom $ showType x
getType _ (multiArgs@(_:_)) = throwError $ NumArgs 1 multiArgs

defNumBoolBinop :: (Integer -> Integer -> Bool) -> LispFunction
defNumBoolBinop op _ [Number a, Number b] = return $ LispBool $ op a b

defBoolBoolBinop :: (Bool -> Bool -> Bool) -> LispFunction
defBoolBoolBinop op _ listArgs = return $ LispBool $ foldl1 op (map extractVal listArgs)
                                where extractVal (LispBool x) = x

defstrBoolBinop :: (String -> String -> Bool) -> LispFunction
defstrBoolBinop op _ [LispString a, LispString b] = return $ LispBool $ op a b

numBoolBinop :: (Integer -> Integer -> Bool) -> LispFunction
numBoolBinop op env listArgs = (defNumBoolBinop op env) =<< (typeIsNumber) =<< (countArgs 2 listArgs)

boolBoolBinop :: (Bool -> Bool -> Bool) -> LispFunction
boolBoolBinop op env listArgs = (defBoolBoolBinop op env) =<< (typeIsBool) =<< (nonzeroArgs listArgs)

strBoolBinop :: (String -> String -> Bool) -> LispFunction
strBoolBinop op env listArgs = (defstrBoolBinop op env) =<< (typeIsString) =<< (countArgs 2 listArgs)

nonzeroArgs :: [LispVal] -> IOThrowsError [LispVal]
nonzeroArgs list@[] = throwError $ NumArgs 1 list
nonzeroArgs list = return list

defCar :: LispFunction
defCar _ [List (x:xs)] = return x
defCar _ [DottedList (x:xs) _] = return x
defCar _ [badArg] = throwError $ TypeMismatch "pair" badArg

car :: LispFunction
car env listArgs = defCar env =<< (countArgs 1 listArgs)

defCdr :: LispFunction
defCdr _ [List (x:xs)] = return $ List xs
defCdr _ [DottedList [x] y] = return y
defCdr _ [DottedList (x:xs) y] = return $ DottedList xs y
defCdr _ [badArg] = throwError $ TypeMismatch "pair" badArg

cdr :: LispFunction
cdr env listArgs = defCdr env =<< (countArgs 1 listArgs)

cons :: LispFunction
cons _ [x, List xs] = return $ List (x:xs)
cons _ [x, DottedList xs ys] = return $ DottedList (x:xs) ys
cons _ [x, y] = return $ DottedList [x] y
cons _ badArgs = throwError $ NumArgs 2 badArgs

eqv :: LispFunction
eqv _ [a,b] = return $ LispBool $ a==b
eqv _ badArgs = throwError $ NumArgs 2 badArgs

defContains :: LispFunction
defContains _ [x, List []] = return $ LispBool False
defContains env [x, List (y:ys)]
                            | x==y = return $ LispBool True
                            | otherwise = defContains env [x, List ys]
defContains _ badArgs = throwError $ TypeMismatch "LispVal and List" (List badArgs)

listContains :: LispFunction
listContains env listArgs = defContains env =<< (countArgs 2 listArgs)

-- Argument sanity checkers

countArgs :: Integer -> [LispVal] -> IOThrowsError [LispVal]
countArgs numArgs listArgs = case (toInteger $ length listArgs) == numArgs of
                                    True ->   return listArgs
                                    False -> throwError $ NumArgs numArgs listArgs

typeIsNumber :: [LispVal] -> IOThrowsError [LispVal]
typeIsNumber listArgs = case (dropWhile (\x -> isLispNumber x) listArgs) of
                                (found:xs) -> throwError $ TypeMismatch "Number" found
                                otherwise -> return listArgs
                             where isLispNumber x = case x of
                                                Number _ -> True
                                                otherwise -> False

typeIsBool :: [LispVal] -> IOThrowsError [LispVal]
typeIsBool listArgs = case (dropWhile (\x -> isBool x) listArgs) of
                                (found:xs) -> throwError $ TypeMismatch "LispBool" found
                                otherwise -> return listArgs
                             where isBool x = case x of
                                                LispBool _ -> True
                                                otherwise -> False

typeIsString :: [LispVal] -> IOThrowsError [LispVal]
typeIsString listArgs = case (dropWhile (\x -> isString x) listArgs) of
                                (found:xs) -> throwError $ TypeMismatch "LispString" found
                                otherwise -> return listArgs
                             where isString x = case x of
                                                LispString _ -> True
                                                otherwise -> False
