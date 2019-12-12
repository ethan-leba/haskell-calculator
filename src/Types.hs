module Types where

data Operator =
    Add |
    Subtract |
    Multiply |
    Divide deriving Show

data AST a =
    Leaf a |
    Node (AST a) Operator (AST a)
