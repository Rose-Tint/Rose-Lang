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
import Data.Table
import Text.Pretty
import Typing.Infer
import Typing.TypeEnv


data TypeCheck = TypeCheck {
    tcErrors :: [ErrInfo],
    tcEnv :: TypeEnv
    }


analyze :: String -> [Expr] -> Analysis Table
analyze name = runAnalyzer name . mapM validate

typeCheck :: [Expr] -> TypeCheck
typeCheck exprs = unInf inf newState okay err
    where
        okay s x = TypeCheck (stErrors s) x
        err s _ -> TypeCheck (stErrors s) emptyEnv
        inf = foldM inferExpr exprs

analyzeExprs :: [Expr] -> BuilderIO (Analysis Table)
analyzeExprs es = do
    name <- getModule
    debug ("Analyzing ["+|name|+"]\n")
    let env = handleTypeCheck $ typeCheck es
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
