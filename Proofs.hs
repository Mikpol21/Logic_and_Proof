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

------  Sequent Calculus  ------

data Proof = Proof Conjecture Rule
data Rule = Axiom
    | AndL Proof        | AndR Proof Proof
    | OrL Proof Proof   | OrR Proof
    | ImpL Proof Proof  | ImpR Proof
    | NotL Proof        | NotR Proof
    | EqvR Proof Proof

--- ∨ ¬  ∧ → ↔ ⊥

instance Show Rule where
    show r = concat (showRule r)
       

showRule :: Rule -> [String]
showRule r =  case r of
    Axiom -> [" from Axioms\n"]
    AndL p -> f "L ∧" : showProof p
    (AndR p q) -> f "R ∧" :  map ('\t':) (showProof p ++ showProof q)
    (OrL p q)  -> f "L ∨" : map ('\t':) (showProof p ++ showProof q)
    (OrR p )  -> f "R ∨" : showProof p
    (ImpL p q) -> f "L →" : map ('\t':) (showProof p ++ showProof q)
    (ImpR p ) -> f "R →" : showProof p
    (NotL p ) -> f "L ¬" : showProof p
    (NotR p ) -> f "R ¬" : showProof p
    (EqvR p q) -> f "R ↔" : map ('\t':) (showProof p ++ showProof q)
    where f xs = " applying " ++ xs ++ " rule\n"

showProof :: Proof -> [String]
showProof (Proof conj r) = (show conj ++ "\t" ++  head rs) : tail rs 
    where rs = showRule r
instance Show Proof where
    show p = concat (zipWith (++) (map ((++".\t") . show) [1..]) (showProof p)) --show conj ++ show r



prove :: Conjecture -> Proof
prove (ps `Proves` qs) = proveKernel
    (Set.fromList $ map extractAtomic a) propLeft
    (map extractAtomic c) propRight
        where
            divide xs = (filter isAtomic xs, filter (not.isAtomic) xs)
            isAtomic (Atomic _) = True
            isAtomic _ = False
            extractAtomic (Atomic a) = a
            (a, propLeft) = divide ps
            (c, propRight) = divide qs
proveKernel :: Set AtomName -> [Prop] -> [AtomName] -> [Prop] -> Proof
proveKernel as [] cs [] = 
    if any (`Set.member` as) cs 
        then Proof (map Atomic (Set.toList as) `Proves` map Atomic cs) Axiom
        else error "Conjecture is false"
proveKernel as ps cs (q:qs) = 
    let conj = (map Atomic (Set.toList as) ++ ps) `Proves` ( map Atomic cs ++ (q:qs))
    in case q of
        Atomic s -> proveKernel as ps (s:cs) qs
        Not pr -> Proof conj . NotR $ 
            proveKernel as (pr:ps) cs qs
        And pr pr' -> Proof conj .  
            AndR (proveKernel as ps cs (pr:qs)) $ proveKernel as ps cs (pr':qs)
        Or pr pr' -> Proof conj . OrR $
            proveKernel as ps cs (pr':pr:qs)
        Imp pr pr' -> Proof conj . ImpR $
            proveKernel as (pr:ps) cs (pr':qs)
        Eqv pr pr' -> Proof conj . EqvR (proveKernel as (pr:ps) cs (pr':qs)) $ proveKernel as (pr':ps) cs (pr:qs)
        Contradiction -> proveKernel as ps cs qs
proveKernel as (p:ps) cs [] = let conj = (map Atomic (Set.toList as) ++ (p:ps)) `Proves` map Atomic cs
    in case p of
        Atomic s -> proveKernel (Set.insert s as) ps cs []
        Not pr -> Proof conj . NotL $ 
            proveKernel as ps cs [pr]
        And pr pr' -> Proof conj . AndL $ 
            proveKernel as (pr:pr':ps) cs []
        Or pr pr' -> Proof conj . 
            OrL (proveKernel as (pr:ps) cs []) $ proveKernel as (pr':ps) cs []
        Imp pr pr' -> Proof conj . 
            ImpL (proveKernel as ps cs [pr]) $ proveKernel as (pr':ps) cs []
        Eqv pr pr' -> proveKernel as ((Not pr `Or` pr'):(pr `Or` Not pr'):ps) cs []
        Contradiction -> error "Found contradiction in antecedents"

proofOf :: String -> Proof
proofOf str = prove $ runParser conjectureParser  str