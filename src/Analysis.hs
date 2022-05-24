module Analysis (
    runAnalysis,
) where

import Control.Monad (forM_)

import Analysis.Error
import AST.Expr
import Builder
import Data.Table
import Text.Pretty
import Typing.Inferable


writeObjFile :: Table -> Builder ()
writeObjFile tbl = writeBin ".o" tbl

runAnalysis :: Table -> [Expr] -> Builder Table
runAnalysis tbl exprs = do
    name <- gets moduleName
    debug ("Analyzing ["+|name|+"]\n")
    let (tbl', errs) = inferTopLevel tbl exprs
    printAnalysisErrors errs
    traceFile "Symbol-Table.txt" tbl'
    writeObjFile tbl'
    return tbl'

printAnalysisErrors :: [ErrInfo] -> Builder ()
printAnalysisErrors [] = return ()
printAnalysisErrors es = do
    lns <- gets (lines . sourceCode)
    name <- gets moduleName
    forM_ es $ \e -> message $ "\n"+|name|+|(lns, e)|+"\n"
