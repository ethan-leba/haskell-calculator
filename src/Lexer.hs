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

-- data Token =
--     Value Double |
--     Paren Paren |
--     Operator Operator
--     deriving Show
-- lexString :: String -> AST

removeWhitespace :: [Char] -> [Char]
removeWhitespace = filter isNotWhitespace
  where
    isNotWhitespace :: Char -> Bool
    isNotWhitespace c = not (c == ' ' || c == '\n')

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

tokenizeList :: [String] -> Maybe [Intermediate Double]
tokenizeList lst = if all isJust converted
    then Just $ map fromJust converted
    else Nothing
  where
    tokenize :: String -> Maybe (Intermediate Double)
    tokenize s | isJust numeric  = Just $ AST (LeafValue (fromJust numeric))
               | isJust operator = Just $ Operand $ fromJust operator
               | isJust paren    = Just $ Paren $ fromJust paren
               | otherwise       = Nothing
      where
        numeric  = readMaybe s :: Maybe Double
        operator = parseOperator (head s)
        paren    = parseParen (head s)
    converted = map tokenize lst

parseRawString :: String -> Maybe [Intermediate Double]
parseRawString = tokenizeList . splitByNum . removeWhitespace

data Intermediate a =
    AST (AST a) |
    Operand Operator |
    Paren Paren
    deriving Show

data ParenMatching =
    Looking |
    NotLooking

strToAST :: String -> Maybe (AST Double)
strToAST s = maybe Nothing (intermediateToAST . processIntermediate) tokens
    where tokens = parseRawString s

processIntermediate :: [Intermediate Double] -> [Intermediate Double]
processIntermediate tokens = pemdas (go tokens [] NotLooking)
  where
    go
        :: [Intermediate Double]
        -> [Intermediate Double]
        -> ParenMatching
        -> [Intermediate Double]
    -- Base case
    go []                  _   NotLooking = []
    go []                  acc Looking    = acc
    -- Paren cases
    go (Paren Open   : xs) _   NotLooking = go xs [] Looking
    go (Paren Closed : xs) acc Looking    = (pemdas acc) ++ go xs [] NotLooking
    -- Value and operator cases
    go (x            : xs) acc Looking    = go xs (acc ++ [x]) Looking
    go (x            : xs) _   NotLooking = x : go xs [] NotLooking

pemdas :: [Intermediate Double] -> [Intermediate Double]
pemdas =
    ( reduceIntermediate Subtract
    . reduceIntermediate Add
    . reduceIntermediate Multiply
    . reduceIntermediate Divide
    )

reduceIntermediate :: Operator -> [Intermediate Double] -> [Intermediate Double]
reduceIntermediate = go
  where
    go :: Operator -> [Intermediate Double] -> [Intermediate Double]
    -- Base cases
    go _  []        = []
    go _  [AST ast] = [AST ast]
    -- Main case
    go op (((AST l) : Operand o : lst@((AST r) : xs))) = if op == o
        then go op ((AST (Node l o r) : xs))
        else (AST l : Operand o : (go op lst))
    -- *Error cases*
    go op (x : xs) = x : go op xs

intermediateToAST :: [Intermediate a] -> Maybe (AST a)
intermediateToAST [AST ast] = Just ast
intermediateToAST _         = Nothing

