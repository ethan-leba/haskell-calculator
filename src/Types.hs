module Types where

data Operator =
    Add |
    Subtract |
    Multiply |
    Divide deriving (Show, Eq)

data AST a=
    LeafValue a |
    Node (AST a) Operator (AST a)
    deriving Show
