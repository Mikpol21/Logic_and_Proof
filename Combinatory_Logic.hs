{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Combinatory_Logic where
import Data.Map (Map)
import qualified Data.Map as Map

data Tree a = Node { value  :: a
                   , children  :: [Tree a] } 

instance Functor Tree where
    fmap f (Node a xs) = Node (f a) $ map (fmap f) xs

foldTree :: (a -> [b] -> b) -> Tree a -> b

foldTree f (Node x xs) = 
    f x $ map (foldTree f) xs

isLeaf :: Tree a -> Bool
isLeaf a = null $ children a
append :: Tree a -> Tree a -> Tree a
append (Node x xs) y = Node x (xs ++ [y])

data Combinator a = Combinator Name ([Term a] -> Term a)
type Term a = Tree (Combinator a)
type Name = String

kestrel :: Combinator a

kestrel = 
    Combinator "K" $ \ as -> case as of
        (x:y:trs) -> x `append` trs
        _ -> 

bluebird :: Combinator a

bluebird = 
    Combinator "B" $ \ as -> case as of
        (x:y:z:trs) -> x:(Node )

