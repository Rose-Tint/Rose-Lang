module Middle.Analyzer (
    module Middle.Analyzer.Error,
    Analysis(..),
    analyze,
) where

import Middle.Analyzer.Checker
import Middle.Analyzer.Error
import Middle.Analyzer.Internal
import Front.Parser.Data (Expr)


analyze :: [Expr] -> Analysis
analyze = runAnalyzer . mapM infer
