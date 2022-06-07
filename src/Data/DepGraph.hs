module Data.DepGraph (
    Scheduling(..),
    scheduleDepGraph,
) where

import Data.Graph

-- import Common.Id


data Scheduling v
    -- | Modules to compile in order
    = Depends [v]
    -- | Modules that form a circular dependency
    | Circular [v]


scheduleDepGraph :: Ord k => [(v, k, [k])] -> Scheduling v
{-# INLINABLE scheduleDepGraph #-}
scheduleDepGraph edges' =
    let (gr, nfv) = graphFromEdges' edges'
        comps = stronglyConnComp (nfv <$> topSort gr)
    in foldr (\comp prev -> case prev of
        Depends vs -> case comp of
            AcyclicSCC v -> Depends (v:vs)
            CyclicSCC vs' -> Circular vs'
        Circular _ -> prev) (Depends []) comps
