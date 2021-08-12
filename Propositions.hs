module Propositions where


import Data.Set (Set)
import qualified Data.Set as Set


type AtomName = String
data Conjecture = Proves {premisses :: [Prop] , conclusion :: Prop} deriving Eq
toProp :: Conjecture -> Prop
toProp (ps `Proves` q) = foldr And (Not Contradiction) ps `Imp` q

-------- Representation of a Proposition
data Prop =   Atomic AtomName
            | Not Prop
            | And Prop Prop
            | Or Prop Prop
            | Imp Prop Prop
            | Eqv Prop Prop
            | Contradiction
        deriving Eq

----- Precedence
--  1. Parethesis
--  2. Negation
--  3. Conjuction
--  4. Disjunction
--  5. Implication
--  6. Equivalence

instance Show Prop where
    show (Atomic xs) = xs
    show (Not prop) = "¬" ++ parensAddition (Not prop) prop
    show (And p q) = parensAddition (And p q) p ++ " ∨ " ++ parensAddition (And p q) q
    show (Or p q) = parensAddition (Or p q) p ++ " ∧ " ++ parensAddition (Or p q) q
    show (Imp p q) = parensAddition (Imp p q) p ++ " → " ++ parensAddition (Imp p q) q
    show (Eqv p q) = parensAddition (Eqv p q) p ++ " ↔ " ++ parensAddition (Eqv p q) q
    show Contradiction = "⊥" 

instance Show Conjecture where
    show (Proves [] y) = " ⊢ " ++ show y
    show (Proves (x:xs) y) = show x ++ concatMap ((", "++).show) xs ++ " ⊢ " ++ show y

parensAddition :: Prop -> Prop -> String
parensAddition a c = if same a c then show c else parens (show c)
    where   parens s = "(" ++ s ++ ")"
            same :: Prop -> Prop -> Bool
            same _ (Not _) = True
            same (And _ _) (And _ _) = True
            same (Or _ _) (Or _ _) = True
            same (Imp _ _) (Imp _ _) = True
            same (Eqv _ _) (Eqv _ _) = True
            same _ (Atomic _) = True
            same _ _ = False


foldProp :: (String -> b) -> (b -> b) -> (b -> b -> b) -> b -> Prop -> b
foldProp atomic no bin c prop = case prop of
    Atomic p -> atomic p
    Contradiction -> c
    Not p -> no $ f p
    p `And` q -> bin (f p) (f q)
    p `Or` q -> bin (f p) (f q)
    p `Imp` q -> bin (f p) (f q)
    p `Eqv` q -> bin (f p) (f q)
    where f = foldProp atomic no bin c

atomicNames :: Prop -> Set AtomName
atomicNames = foldProp Set.singleton id Set.union Set.empty


eval :: (AtomName -> Bool) -> Prop -> Bool
eval v prop = case prop of
    Contradiction -> False
    Atomic p -> v p
    Not p -> not $ eval v p
    p `And` q -> eval v p && eval v q
    p `Or` q -> eval v p || eval v q
    p `Imp` q -> not p' || (p' && eval v q)
        where p' = eval v p
    p `Eqv` q -> eval v p == eval v q


