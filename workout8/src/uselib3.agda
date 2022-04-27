module uselib3 where

open import lib

-- see comment at top of uselib1.agda

{- hint: _+_ is parsed left associatively, so if you see
   
   a + b + c

   please remember this is (a + b) + c.  This affects how you apply +assoc at the end
   to get the summands in exactly the same grouping. -}

lem : ∀(a b c d : ℕ) → (a + (b + c)) + d ≡ (a + b) + (c + d)
lem a b c d rewrite +assoc a b c | +assoc (a + b) c d = refl

poly : ∀(x y : ℕ) → square x + 2 * x * y + square y ≡ square (x + y)
poly x y rewrite +0 x | *distribr x x y | *distribr x y (x + y)
 | *distribl x x y | *distribl y x y | +assoc (x * y) (y * x) (y * y)
 | *comm y x | lem (x * x) (x * y) (x * y) (y * y) =  refl




