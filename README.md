# Logic_and_Proof

### Automated Propositional Logic theorem prover

####My code parses any propositional logic conjectures and either disproves them or presents the proof using sequent calculus.
####For exmaple:
For conjecture
(a or b), not b proves a

My code returns following proof:
1.	a ∨ b, ¬b ⊢ a 	applying L ∨ rule (2,4)
2.		a, ¬b ⊢ a 	applying L ¬ rule (3)
3.		a ⊢ b, a from Axioms
4.		b, ¬b ⊢ a 	applying L ¬ rule (5)
5.		b ⊢ b, a from Axioms
