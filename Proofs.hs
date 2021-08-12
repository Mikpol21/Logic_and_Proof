{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Logic_Parser
import Propositions
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.Types.Lens (_Impl)

cartesian :: [[a]] -> [[a]]
cartesian [] = [[]]
cartesian (xs:xss) = do
    x <- xs
    ys <- cartesian xss
    return $ x : ys


------ Brute Force -------
type Valuation = Map AtomName Bool
powerBoolSet :: Int -> [[Bool]]
powerBoolSet n = cartesian (replicate n [False, True])

allValuations :: Prop -> [Valuation]
allValuations p = map Map.fromList atomsWithBools
    where   atoms = Set.toList $ atomicNames p
            atomsWithBools = map (zip atoms) . powerBoolSet $ length atoms


bruteIsTheorem :: Conjecture -> Bool
bruteIsTheorem c = all (flip eval p . (Map.!)) $ allValuations p where p = toProp c


solve :: String -> Bool
solve = bruteIsTheorem . runParser conjectureParser

type RuleName = String
data ProofTree = InferBy RuleName [ProofTree] Prop