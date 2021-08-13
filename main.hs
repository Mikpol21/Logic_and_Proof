import Proofs ( proofOf )
import Propositions

main :: IO ()
main = do
    putStrLn "Input Conjecture"
    x <- getLine
    putStrLn ""
    putStr . show $ proofOf x
    putStrLn ""