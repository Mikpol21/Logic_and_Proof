{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Proofs where
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
    deriving Eq
data Rule = Axiom
    | AndL Proof        | AndR Proof Proof
    | OrL Proof Proof   | OrR Proof
    | ImpL Proof Proof  | ImpR Proof
    | NotL Proof        | NotR Proof
    | EqvR Proof Proof
        deriving Eq


------  Proof visualization  ------
instance Show Proof where
    show p = unlines . addLines $ showProof (`numberOfLine` ps) p
        where   ps = proofToList p [] --show conj ++ show r
                addLines = zipWith (++) (map (( ++ ".\t") . show) [1..])


proofToList :: Proof -> [Proof] -> [Proof]
proofToList (Proof conj rule) xs = Proof conj rule : ruleToList rule xs
ruleToList :: Rule -> [Proof] -> [Proof]
ruleToList r xs = case r of
  Axiom -> xs
  AndL pr -> proofToList pr xs
  AndR pr pr' -> proofToList pr $ proofToList pr' xs
  OrL pr pr' -> proofToList pr $ proofToList pr' xs
  OrR pr -> proofToList pr xs
  ImpL pr pr' -> proofToList pr $ proofToList pr' xs
  ImpR pr -> proofToList pr xs
  NotL pr -> proofToList pr xs
  NotR pr -> proofToList pr xs
  EqvR pr pr' -> proofToList pr $ proofToList pr' xs

showProof :: (Proof -> Int) -> Proof -> [String]
showProof g (Proof conj rule) = case rule of
    Axiom -> [show conj ++ " from Axioms"]
    AndR p q -> (f "R ∧" ++ show (g p , g q)) : addTab (showProof g p ++ showProof g q)
    OrL p q -> (f "L ∨" ++ show (g p , g q)) : addTab (showProof g p ++ showProof g q)
    OrR p -> (f "R ∨" ++ "(" ++ show (g p) ++ ")") : showProof g p
    ImpL p q -> (f "L →" ++ show (g p , g q)) : addTab (showProof g p ++ showProof g q)
    ImpR p -> (f "R →" ++ "(" ++ show (g p) ++ ")") : showProof g p
    NotL p -> (f "L ¬" ++ "(" ++ show (g p) ++ ")") : showProof g p
    NotR p -> (f "R ¬" ++ "(" ++ show (g p) ++ ")") : showProof g p
    EqvR p q -> (f "R ↔" ++ show (g p , g q)) : addTab (showProof g p ++ showProof g q)
    where   f xs = show conj ++ "\tapplying " ++ xs ++ " rule "
            addTab = map ('\t':)

numberOfLine :: Proof -> [Proof] -> Int
numberOfLine (Proof conj rule) ((Proof conj2 rule2):xs) =
    if conj == conj2
        then 1
        else 1 + numberOfLine (Proof conj rule) xs

------ Proving Automation ------

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