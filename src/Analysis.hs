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


-- analyze :: CmdLine -> [Expr] -> ([Expr], Table, [ErrInfo])
-- analyze cmd = runAnalyzer cmd . mapM validate

runAnalysis :: [Expr] -> Builder Table
runAnalysis exprs = do
    name <- gets moduleName
    debug ("Analyzing ["+|name|+"]\n")
    tbl <- case inferTopLevel exprs of
        Left err -> do
            printAnalysisErrors [err]
            return emptyTable
        Right tbl -> return tbl
    trace "Symbol-Table.txt" tbl
    return tbl

printAnalysisErrors :: [ErrInfo] -> Builder ()
printAnalysisErrors [] = return ()
printAnalysisErrors es = do
    lns <- gets (lines . sourceCode)
    name <- gets moduleName
    forM_ es $ \e -> case emError e of
        Right FalseError -> return ()
        _ -> message $ "\n"+|name|+|(lns, e)|+"\n"
