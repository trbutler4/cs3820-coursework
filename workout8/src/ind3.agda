module ind3 where

open import lib
open import ntree

-- see comment at top of ind1.agda

mirror-mirror : ∀ (t : Tree) → mirror (mirror t) ≡ t
mirror-mirror Leaf  = refl
mirror-mirror (Node n t1 t2) rewrite mirror-mirror t1 | mirror-mirror t2 = refl
