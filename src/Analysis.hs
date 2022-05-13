module Analysis (
    Analysis(..),
    analyzeExprs,
) where

import Control.Monad (forM_, foldM)

import Analysis.Analyzer
import Analysis.Error
import Analysis.Validator
import AST.Expr
import Builder
import Text.Pretty
import Typing.Infer as Inf
import Typing.Inferable
import Typing.TypeEnv


data TypeCheck = TypeCheck [ErrInfo] TypeEnv


analyze :: String -> [Expr] -> Analysis
analyze name = runAnalyzer name . mapM validate

typeCheck :: [Expr] -> TypeCheck
typeCheck exprs = runInfer inf okay err
    where
        okay s x = TypeCheck (Inf.stErrors s) x
        err s _ = TypeCheck (Inf.stErrors s) emptyEnv
        inf = foldM inferExpr emptyEnv exprs

analyzeExprs :: [Expr] -> BuilderIO Analysis
analyzeExprs es = do
    name <- getModule
    debug ("Analyzing ["+|name|+"]\n")
    _env <- handleTypeCheck (typeCheck es)
    let res = analyze name es
    trace "Symbol-Table.txt" (arTable res)
    printAnalysisErrors $ arErrors res
    return res

printAnalysisErrors :: [ErrInfo] -> BuilderIO ()
printAnalysisErrors [] = return ()
printAnalysisErrors es = do
    lns <- lines <$> getSource
    name <- getModule
    forM_ es $ \e -> case emError e of
        Right FalseError -> return ()
        _ -> message $ "\n"+|name|+|(lns, e)|+"\n"

handleTypeCheck :: TypeCheck -> BuilderIO TypeEnv
handleTypeCheck (TypeCheck errs env) = do
    printAnalysisErrors errs
    return env
