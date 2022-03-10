module Analyzer.Typing where

import Parser.Data


integral :: Constraint
integral = (Var "Integral" (-1) (-1), )
