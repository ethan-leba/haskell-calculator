module Evaluator where

import           Types

evaluate :: Fractional a => AST a -> a
evaluate (LeafValue a) = a
evaluate (Node l op r) = (getOperatorFun op) (evaluate l) (evaluate r)

getOperatorFun :: Fractional a => Operator -> (a -> a -> a)
getOperatorFun op = case op of
    Add      -> (+)
    Subtract -> (-)
    Multiply -> (*)
    Divide   -> (/)
