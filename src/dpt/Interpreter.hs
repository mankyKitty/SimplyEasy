module Interpreter where

data Stmt i tinf = Let String i -- let x = t
                 | Assume [(String, tinf)] -- assume x :: *, x :: t
                 | Eval i
                 deriving (Show)
