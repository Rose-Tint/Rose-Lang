module Typing.Rules where

import Analyzer.Analyzer (Analyzer)
import Analyzer.SymbolTable
import Parser.Data hiding (Expr)
import qualified Parser.Data as PD (Expr)
import SymbolTable



type Env = SymbolTable


data Expr
    = Expr PD.Expr
    | Value Value
    | Data SymbolData


data Premise
    = Proof (Maybe Premise) Expr
    | Imply Premise Premise -- implication


type Conclusion = SymbolData


newtype Rule = Rule {
        unRule :: Env
            -> (Env -> Premise -> Conclusion)
            -> Conclusion
    }


-- "A |- B" if and only if "|- A -> B"
-- IOW, "A" proves "B" if and only if the proof
-- of "A" implies "B"
proves :: Premise -> Premise -> Bool
proves


-- sequent (prooves that)
infix 3 |-
(|-) :: Maybe Premise -> Expr -> Premise
(|-) = Proof


(==>) :: Premise -> Premise -> Visitor Premise
Proof mp e ==> 


application :: Variable -> Value -> Analyzer Rule
application func _ = do
    dta <- Data <$> searchScopeds func
    return $! Rule $ \env rul -> rul env $! Nothing |- dta
