import Proofs
import Propositions

main = do
    putStrLn "Input Conjecture"
    x <- getLine
    putStr . show $ proofOf x