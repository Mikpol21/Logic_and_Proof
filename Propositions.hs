module Propositions where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


type AtomName = String
type Valuation = AtomName -> Bool
type BinOp a = a -> a -> a 

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
    show (Not prop) = "~" ++ show prop
    show (And p q) = "(" ++ show p ++ " & " ++ show q ++ ")"
    show (Or p q) = "(" ++ show p ++ " || " ++ show q ++ ")"
    show (Imp p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
    show (Eqv p q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"
    show Contradiction = "F"

foldProp :: (String -> b) -> (b -> b) -> BinOp b -> b -> Prop -> b
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


eval :: Valuation -> Prop -> Bool
eval v prop = case prop of
    Contradiction -> False
    Atomic p -> v p
    Not p -> not $ eval v p
    p `And` q -> eval v p && eval v q
    p `Or` q -> eval v p || eval v q
    p `Imp` q -> not p' || (p' && eval v q)
        where p' = eval v p
    p `Eqv` q -> eval v p == eval v q

satisfying :: Prop -> [Valuation] -> [Valuation]
satisfying p = filter $ flip eval p
