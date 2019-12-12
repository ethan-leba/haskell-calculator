module Lexer where

import           Types
import           Data.Char                      ( isDigit )
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                )
import           Text.Read                      ( readMaybe )

data Paren =
    Open |
    Closed
    deriving Show

data Token =
    Value Double |
    P Paren |
    Op Operator
    deriving Show
-- lexString :: String -> AST

removeWhitespace :: [Char] -> [Char]
removeWhitespace = filter isNotWhitespace
  where
    isNotWhitespace :: Char -> Bool
    isNotWhitespace c = not (c == ' ' || c == '\n')


tokenizeList :: [String] -> Maybe [Token]
tokenizeList lst = if all isJust converted
    then Just $ map fromJust converted
    else Nothing
  where
    tokenize :: String -> Maybe Token
    tokenize s | isJust numeric  = Just $ Value (fromJust numeric)
               | isJust operator = Just $ Op $ fromJust operator
               | isJust paren    = Just $ P $ fromJust paren
               | otherwise       = Nothing
      where
        numeric  = readMaybe s :: Maybe Double
        operator = parseOperator (head s)
        paren    = parseParen (head s)
    converted = map tokenize lst


splitByNum :: String -> [String]
splitByNum = foldr concatIfNum [""]
  where
    shouldConcat :: Char -> [String] -> Bool
    shouldConcat c ([]      : xs) = isDigit c
    shouldConcat c ((x : _) : _ ) = (isDigit c && isDigit x)

    concatIfNum :: Char -> [String] -> [String]
    concatIfNum c lst@(x : xs) =
        if shouldConcat c lst then ((c : x) : xs) else [c] : lst

parseParen :: Char -> Maybe Paren
parseParen c = case c of
    '(' -> Just Open
    ')' -> Just Closed
    _   -> Nothing

parseOperator :: Char -> Maybe Operator
parseOperator c = case c of
    '+' -> Just Add
    '-' -> Just Subtract
    '*' -> Just Multiply
    '/' -> Just Divide
    _   -> Nothing
