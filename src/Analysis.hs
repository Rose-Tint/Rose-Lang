module Analysis (
    runAnalysis,
) where

import Control.Monad (forM_)

import Analysis.Analyzer hiding (gets)
import Analysis.Error
import Analysis.Validator
import AST.Expr
import Builder
import Cmd (CmdLine)
import Data.Table (Table)
import Text.Pretty


analyze :: CmdLine -> [Expr] -> ([Expr], Table, [ErrInfo])
analyze cmd = runAnalyzer cmd . mapM validate

runAnalysis :: [Expr] -> Builder ([Expr], Table)
runAnalysis es = do
    name <- gets moduleName
    cmd <- ask
    debug ("Analyzing ["+|name|+"]\n")
    let (es', tbl, errs) = analyze cmd es
    trace "Symbol-Table.txt" tbl
    printAnalysisErrors errs
    return (es', tbl)

printAnalysisErrors :: [ErrInfo] -> Builder ()
printAnalysisErrors [] = return ()
printAnalysisErrors es = do
    lns <- gets (lines . sourceCode)
    name <- gets moduleName
    forM_ es $ \e -> case emError e of
        Right FalseError -> return ()
        _ -> message $ "\n"+|name|+|(lns, e)|+"\n"
