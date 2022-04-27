module Exercises2 where
import Extralib
import Reader ( lrAdd, lrElem, ListReader )
import Term
import Data.Tuple (swap)
import Language.Haskell.TH (Exp(ListE))
import GHC.ResponseFile (unescapeArgs)

{- return the list of free variables in the given
   Expr.  The input list of variables is a list of
   variables that this call to freeVars should consider
   bound.  So if you encounter a variable in that list,
   it is not free at that point.  You can add a variable
   to that list to mark it as bound in a recursive call.

   The returned list may contain duplicates.  These are removed
   below.
-}
freeVarsh :: [Var] -> Term -> [Var]
freeVarsh vs (Var v) = if elem v vs then [] else [v] -- if not in vs, it is a free variable 
freeVarsh vs (App t1 t2) = (freeVarsh vs t1) ++ (freeVarsh vs t2)
freeVarsh vs (Lam v t) = freeVarsh (v:vs) t-- add variable to list of bound variables

freeVars :: Term -> [Var]
freeVars t = canonOrd (freeVarsh [] t)

-- rewrite freeVarsh using the ListReader monad (Reader.hs)
freeVarsh' :: Term -> ListReader Var [Var]
freeVarsh' (Var v) = undefined 
freeVarsh' (App t1 t2) = undefined 
freeVarsh' (Lam v t) = undefined

-- use freeVarsh' and canonOrd to get a list of free variables without duplicates
freeVars' :: Term -> [Var]
freeVars' = undefined

{- given variables x, y, and z, check to see if z is either x or y.  If it is
   return the other one (so if z is x, return y; if z is y, return x).  If
   z is neither x nor y, return z. -}
swapVar :: Var -> Var -> Var -> Var
swapVar x y z
  | z == x = y
  | z == y = x
  | otherwise = z


-- using swapVar, change x into y and vice versa everywhere in the Term,
-- including at binding occurrences (first argument to Lam). 
swapNames :: Var -> Var -> Term -> Term
swapNames x y (Var v) = Var (swapVar x y v)
swapNames x y (App t1 t2) = App (swapNames x y t1) (swapNames x y t2)
swapNames x y (Lam v t) = Lam (swapVar x y v) (swapNames x y t) 

{- findNewName vs v

   Given list of variables vs (presumed to be finite),
   find a new version of v that is not in vs, by adding
   single quotation marks ' to the end of v.  If v itself
   is not in vs already, then just return it (no ' marks added) -}
findNewName :: [Var] -> Var -> Var
findNewName vs v = 
   if not (elem v vs)
      then v 
   else
      findNewName vs (v ++ "'") 

{- using findNewName and swapNames, safely rename all bound
   variables in the given Term to new names that are
   not in the given list of variables.

   The given list of variables may be assumed to
   contain all free variables of the given Term, and you
   should maintain that property when you recurse, so that
   new names you pick will not conflict with names that are
   bound deeper in the term. -}
renameAwayFrom :: [Var] -> Term -> Term
renameAwayFrom vs (Var v) = let n = Var (findNewName vs v) in swapNames v v n
renameAwayFrom vs (App t1 t2) = App (renameAwayFrom vs t1) (renameAwayFrom vs t2)
renameAwayFrom vs (Lam v t) = Lam v (renameAwayFrom vs t)