-- Imports and the main function

module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Applicative hiding ((<|>), many)
import qualified Data.Map as Map
import Numeric
import Control.Monad.Except

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)

-- LispVal datatype

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Gaussian (Integer, Integer)
             | LispString String
             | LispChar Char
             | LispBool Bool

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

-- Parsers and parser combinators

symbol :: Parser Char
symbol = oneOf "!$#%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> LispString $ "No match: " ++ show err
    Right val -> val

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
parseNumber = do x <- try decimal <|> try octal <|> try hexadecimal <|> number
                 return $ (Number . read) x
              where number = many1 digit
                    decimal = string "#d" *> number
                    octal = string "#o" *> fmap (show . fst . head . readOct) (many1 octDigit)
                    hexadecimal = string "#x" *> fmap (show . fst . head . readHex) (many1 hexDigit)

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

eval :: LispVal -> LispVal
eval val@(LispString _) = val
eval val@(Number _) = val
eval val@(Gaussian _) = val
eval val@(LispBool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func: args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (LispBool False) ($ args) $ lookup func primitives

-- Inbuilt language functions

primitives :: [(String, [LispVal] -> LispVal)]
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
                ("class-of", (\x -> Atom $ (showType . head)(x)))
             ]

numericBinop :: (Integer -> Integer -> Integer) -> ([LispVal] -> LispVal)
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

isLispString :: [LispVal] -> LispVal
isLispString (LispString _:_) = LispBool True
isLispString _ = LispBool False

isNumber :: [LispVal] -> LispVal
isNumber (Number _:_) = LispBool True
isNumber _ = LispBool False
