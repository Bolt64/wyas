-- Imports and the main function

module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Applicative hiding ((<|>), many)
import qualified Data.Map as Map
import Numeric
import Control.Monad.Error

main :: IO ()
main = do
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
        putStrLn $ extractValue $ trapError evaled

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

-- LispVal datatype

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Gaussian (Integer, Integer)
             | LispString String
             | LispChar Char
             | LispBool Bool

-- Show typeclass instantiation

instance Show LispVal where
    show lispval = case lispval of
                    Atom s -> s
                    Number n -> show n
                    Gaussian (real, imag) -> (show real) ++ "i + " ++ (show imag) ++ "j"
                    LispString s ->  "\"" ++ s ++ "\""
                    LispChar c -> "\'" ++ [c] ++ "\'"
                    LispBool b -> case b of
                                    True -> "#t"
                                    False -> "#f"
                    List l -> ((wrapWithBrackets . showCleanList) l)
                    DottedList l e -> ((wrapWithBrackets . showCleanList) l) ++ " " ++ (show e)

showType :: LispVal -> String
showType (Atom _) = "Atom"
showType (List _) = "List"
showType (DottedList _ _) = "DottedList"
showType (Number _) = "Number"
showType (Gaussian _) = "Gaussian"
showType (LispString _) = "LispString"
showType (LispChar _) = "LispChar"
showType (LispBool _) = "LispBool"

-- Some helper functions

wrapWithBrackets :: String -> String
wrapWithBrackets s = "(" ++ s ++ ")"

showCleanList :: [LispVal] -> String
showCleanList = unwords . map show

-- Show typeclass instantiation

instance Eq LispVal where
    (==) = areEqual

areEqual :: LispVal -> LispVal -> Bool
areEqual (Atom a) (Atom b) = a==b
areEqual (Number a) (Number b) = a==b
areEqual (Gaussian (a,ai)) (Gaussian (b,bi)) = a==b && ai==bi
areEqual (LispString a) (LispString b) = a==b
areEqual (LispChar a) (LispChar b) = a==b
areEqual (LispBool a) (LispBool b) = a==b
areEqual (List []) (List []) = True
areEqual (List (x:xs)) (List []) = False
areEqual (List []) (List (x:xs)) = False
areEqual (List (x:xs)) (List (y:ys)) = x==y && (areEqual (List xs) (List ys))
areEqual (DottedList xs x) (DottedList ys y) = (areEqual x y) && (areEqual (List xs) (List ys))
areEqual _ _ = False

-- LispError datatype

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ (show form)
showError (NotFunction message func) = message ++ ": " ++ (func)
showError (NumArgs expected found) = "Expected " ++ (show expected) ++ " args: found values " ++ (showCleanList found)
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ (show found)
showError (Parser parseErr) = "Parse error at " ++ (show parseErr)

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- Parsers and parser combinators

symbol :: Parser Char
symbol = oneOf "!$#%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapeCharacters = ['\\', '"', 'n', 't', 'r']

transformChar :: Char -> Char
transformChar x = case x of
                    'n' -> '\n'
                    't' -> '\t'
                    'r' -> '\r'
                    otherwise -> x

escape :: Parser Char
escape = do slash <- char '\\'
            escapeCode <- oneOf escapeCharacters
            return $ transformChar escapeCode

nonEscape :: Parser Char
nonEscape = noneOf "\\\""

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ nonEscape <|> escape
                 char '"'
                 return $ LispString x

parseChar :: Parser LispVal
parseChar = (\x -> LispChar x) <$> (string "#\\" *> (nonEscape <|> escape))
               
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                        "#t" -> LispBool True
                        "#f" -> LispBool False
                        otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do x <- try negativeNumber <|> try decimal <|> try octal <|> try hexadecimal <|> number
                 return $ (Number . read) x
              where negativeNumber = char '-' *> fmap (show . multNegOne) parseNumber
                    number = many1 digit
                    decimal = string "#d" *> number
                    octal = string "#o" *> fmap (show . fst . head . readOct) (many1 octDigit)
                    hexadecimal = string "#x" *> fmap (show . fst . head . readHex) (many1 hexDigit)
                    multNegOne  (Number x) = (-1)*x

parseGaussian :: Parser LispVal
parseGaussian = do real <- many1 digit
                   char 'i'
                   char '+'
                   imag <- many1 digit
                   char 'j'
                   return $ Gaussian (read real, read imag)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseEitherList :: Parser LispVal
parseEitherList = do char '('
                     x <- (try parseList) <|> parseDottedList
                     char ')'
                     return x

parseExpr :: Parser LispVal
parseExpr = try parseChar
        <|> try parseGaussian
        <|> parseNumber
        <|> parseString
        <|> parseAtom
        <|> parseQuoted
        <|> parseEitherList

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

-- Inbuilt language functions

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