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


analyze :: String -> [Expr] -> Analysis
analyze name = runAnalyzer name . mapM validate

runAnalysis :: [Expr] -> Builder Analysis
runAnalysis es = do
    name <- getModule
    debug ("Analyzing ["+|name|+"]\n")
    let res = analyze name es
    trace "Symbol-Table.txt" (arTable res)
    printAnalysisErrors (arErrors res)
    return res

printAnalysisErrors :: [ErrInfo] -> Builder ()
printAnalysisErrors [] = return ()
printAnalysisErrors es = do
    lns <- gets (lines . sourceCode)
    name <- gets moduleName
    forM_ es $ \e -> case emError e of
        Right FalseError -> return ()
        _ -> message $ "\n"+|name|+|(lns, e)|+"\n"
