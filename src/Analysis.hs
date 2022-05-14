module Analysis (
    Analysis(..),
    runAnalysis,
) where

import Control.Monad (forM_)

import Analysis.Analyzer
import Analysis.Error
import Analysis.Validator
import AST.Expr
import Builder
import Text.Pretty
import Typing.Inferable


analyze :: String -> [Expr] -> Analysis
analyze name = runAnalyzer name . mapM validate

typeCheck :: [Expr] -> BuilderIO ()
typeCheck exprs = do
    let Inf !errs !_env = makeInference exprs
    printAnalysisErrors errs
    return ()

runAnalysis :: [Expr] -> BuilderIO Analysis
runAnalysis es = do
    name <- getModule
    debug ("Analyzing ["+|name|+"]\n")
    _env <- typeCheck es
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
