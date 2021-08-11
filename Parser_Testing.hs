{-# LANGUAGE TupleSections #-}
import Logic_Parser
import System.Random
import Propositions
import Control.Monad.RWS (Functor)
import System.Random.Stateful (Random)

newtype RandomSt a = State { runState :: StdGen -> (a, StdGen)}


instance Monad RandomSt where
    return x = State (x, )
    a >>= f = State $ \g -> let  (x, g1) = runState a g in runState (f x) g1 
instance Functor RandomSt where
    fmap f xs = do
        f <$> xs

instance Applicative RandomSt where
    pure = return
    fs <*> xs = do
        f <- fs
        f <$> xs


randomElement :: [a] -> RandomSt a
randomElement xs = do
    x <- State $ randomR (0, length xs - 1)
    return $ xs !! x

randomProp :: Int -> RandomSt Prop
randomProp n = do
    x <- State $ randomR (0, 99)
    str <- randomElement $ map show ['a'..'z']
    if n <= 0 then return $ Atomic str else distribution x str $ randomProp (n - 1)


distribution :: Int -> String -> RandomSt Prop -> RandomSt Prop
distribution x str f
    | x < 70 = return $ Atomic str
    | x < 55 = And <$> f <*> f
    | x < 75 = Or <$> f <*> f
    | x < 85 = Not <$> f
    | x < 91 = Imp <$> f <*> f
    | x < 96 = Eqv <$> f <*> f
    | otherwise = return Contradiction

