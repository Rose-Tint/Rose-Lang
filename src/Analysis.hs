module Analysis (
    Analysis(..),
    analyzeExprs,
) where

import Control.Monad (forM_)

import Analysis.Analyzer
import Analysis.Error
import Analysis.Validator
import AST.Expr
import Builder
import Text.Pretty


analyze :: [Expr] -> Analysis
analyze = runAnalyzer . mapM validate

analyzeExprs :: [Expr] -> BuilderIO Analysis
analyzeExprs es = do
    name <- getModule
    debug ("Analyzing ["+|name|+"]\n")
    let !res = analyze es
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
