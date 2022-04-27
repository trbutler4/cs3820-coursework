module ind2 where

open import lib

-- see comment at top of ind1.agda

{- first, define the following function, to
   compare two lists lexicographically.  You proceed down the
   lists comparing corresponding elements using p.  Return
   ff iff p returns false at any point.  Otherwise, if
   the first list runs out of letters first, return tt. -}
list-lexcomp : ∀{A B : Set} → (A → B → 𝔹) → 𝕃 A → 𝕃 B → 𝔹
list-lexcomp p xs ys = tt

{- Now prove this theorem about the code you wrote.
   I needed ≤-suc from nat-thms.agda -}
thm2 : ∀(xs : 𝕃 ℕ) → list-lexcomp _≤_ xs (map suc xs) ≡ tt
thm2 xs = refl
