module SourceParser
    (
    parseExpr,
    parse
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import DataTypes
import Numeric (readHex, readOct)
import Control.Monad
import Control.Applicative hiding ((<|>), many)

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
