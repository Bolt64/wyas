module PrimitiveFunctions
    (
    primitives,
    countArgs,
    listContains
    ) where

import DataTypes

type LispFunction = [LispVal] -> ThrowsError LispVal

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
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

isLispString :: LispFunction
isLispString ([LispString _]) = return $ LispBool True
isLispString ([_]) = return $ LispBool False
isLispString (multiArgs@(_:_)) = throwError $ NumArgs 1 multiArgs

isNumber :: LispFunction
isNumber ([Number _]) = return $ LispBool True
isNumber ([_]) = return $ LispBool False
isNumber (multiArgs@(_:_)) = throwError $ NumArgs 1 multiArgs

getType :: LispFunction
getType [x] = return $ Atom $ showType x
getType (multiArgs@(_:_)) = throwError $ NumArgs 1 multiArgs

defNumBoolBinop :: (Integer -> Integer -> Bool) -> LispFunction
defNumBoolBinop op [Number a, Number b] = return $ LispBool $ op a b

defBoolBoolBinop :: (Bool -> Bool -> Bool) -> LispFunction
defBoolBoolBinop op listArgs = return $ LispBool $ foldl1 op (map extractVal listArgs)
                                where extractVal (LispBool x) = x

defstrBoolBinop :: (String -> String -> Bool) -> LispFunction
defstrBoolBinop op [LispString a, LispString b] = return $ LispBool $ op a b

numBoolBinop :: (Integer -> Integer -> Bool) -> LispFunction
numBoolBinop op listArgs = (defNumBoolBinop op) =<< (typeIsNumber) =<< (countArgs 2 listArgs)

boolBoolBinop :: (Bool -> Bool -> Bool) -> LispFunction
boolBoolBinop op listArgs = (defBoolBoolBinop op) =<< (typeIsBool) =<< (nonzeroArgs listArgs)

strBoolBinop :: (String -> String -> Bool) -> LispFunction
strBoolBinop op listArgs = (defstrBoolBinop op) =<< (typeIsString) =<< (countArgs 2 listArgs)

nonzeroArgs :: [LispVal] -> ThrowsError [LispVal]
nonzeroArgs list@[] = throwError $ NumArgs 1 list
nonzeroArgs list = return list

defCar :: LispFunction
defCar [List (x:xs)] = return x
defCar [DottedList (x:xs) _] = return x
defCar [badArg] = throwError $ TypeMismatch "pair" badArg

car :: LispFunction
car listArgs = defCar =<< (countArgs 1 listArgs)

defCdr :: LispFunction
defCdr [List (x:xs)] = return $ List xs
defCdr [DottedList [x] y] = return y
defCdr [DottedList (x:xs) y] = return $ DottedList xs y
defCdr [badArg] = throwError $ TypeMismatch "pair" badArg

cdr :: LispFunction
cdr listArgs = defCdr =<< (countArgs 1 listArgs)

cons :: LispFunction
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs ys] = return $ DottedList (x:xs) ys
cons [x, y] = return $ DottedList [x] y
cons badArgs = throwError $ NumArgs 2 badArgs

eqv :: LispFunction
eqv [a,b] = return $ LispBool $ a==b
eqv badArgs = throwError $ NumArgs 2 badArgs

defContains :: LispFunction
defContains [x, List []] = return $ LispBool False
defContains [x, List (y:ys)]
                            | x==y = return $ LispBool True
                            | otherwise = defContains [x, List ys]
defContains badArgs = throwError $ TypeMismatch "LispVal and List" (List badArgs)

listContains :: LispFunction
listContains listArgs = defContains =<< (countArgs 2 listArgs)

-- Argument sanity checkers

countArgs :: Integer -> [LispVal] -> ThrowsError [LispVal]
countArgs numArgs listArgs = case (toInteger $ length listArgs) == numArgs of
                                    True ->   return listArgs
                                    False -> throwError $ NumArgs numArgs listArgs

typeIsNumber :: [LispVal] -> ThrowsError [LispVal]
typeIsNumber listArgs = case (dropWhile (\x -> isNumber x) listArgs) of
                                (found:xs) -> throwError $ TypeMismatch "Number" found
                                otherwise -> return listArgs
                             where isNumber x = case x of
                                                Number _ -> True
                                                otherwise -> False

typeIsBool :: [LispVal] -> ThrowsError [LispVal]
typeIsBool listArgs = case (dropWhile (\x -> isBool x) listArgs) of
                                (found:xs) -> throwError $ TypeMismatch "LispBool" found
                                otherwise -> return listArgs
                             where isBool x = case x of
                                                LispBool _ -> True
                                                otherwise -> False

typeIsString :: [LispVal] -> ThrowsError [LispVal]
typeIsString listArgs = case (dropWhile (\x -> isString x) listArgs) of
                                (found:xs) -> throwError $ TypeMismatch "LispString" found
                                otherwise -> return listArgs
                             where isString x = case x of
                                                LispString _ -> True
                                                otherwise -> False
