module Middle.Analyzer (
    module Middle.Analyzer.Error,
    Analysis(..),
    analyze,
) where

import Middle.Analyzer.Error
import Middle.Analyzer.Internal
import Middle.Analyzer.Validator
import Front.Parser.Data (Expr)


analyze :: [Expr] -> Analysis
analyze = runAnalyzer . mapM validate
