module DataTypes
    (
      LispVal(..),
      showType,
      LispError(..),
      ThrowsError,
      throwError,
      trapError,
      extractValue
    ) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec (Parser, ParseError)
import Data.IORef

-- Environment for closures
type Env = IORef [(String, IORef LispVal)]

-- LispVal datatype

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Gaussian (Integer, Integer)
             | LispString String
             | LispChar Char
             | LispBool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}

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
                    PrimitiveFunc _ -> "<primitive>"
                    Func {params = args, vararg = varargs, body = body, closure = env} ->
                        "(lambda (" ++ unwords (map show args) ++
                            (case varargs of
                                Nothing -> ""
                                Just arg -> " . " ++ arg) ++ ") ...)"

showType :: LispVal -> String
showType (Atom _) = "Atom"
showType (List _) = "List"
showType (DottedList _ _) = "DottedList"
showType (Number _) = "Number"
showType (Gaussian _) = "Gaussian"
showType (LispString _) = "LispString"
showType (LispChar _) = "LispChar"
showType (LispBool _) = "LispBool"
showType (PrimitiveFunc _) = "PrimitiveFunc"
showType (Func _) = "Func"

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
