module bools4 where

open import lib

----------------------------------------------------------------------
-- some additional problems
----------------------------------------------------------------------

&&-distrib : ∀ x y z → x && (y || z) ≡ (x && y) || (x && z)
&&-distrib tt tt tt = refl
&&-distrib tt tt ff = refl
&&-distrib tt ff ff = refl
&&-distrib tt ff tt = refl
&&-distrib ff ff tt = refl
&&-distrib ff tt ff = refl
&&-distrib ff ff ff = refl
&&-distrib ff tt tt = refl
