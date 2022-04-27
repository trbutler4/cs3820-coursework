module Exercises1 where
import Extralib
import Expr
import Accum
import Distribution.Simple.Utils (xargs)
import Reader (tableLookup)
import Text.XHtml (table)

{- given an expression e, extract all the variable definitions from it.

   In more detail: return a list of all pairs (x,e) where there is a Let-expression
   in e defining x to be e.  Replace all those Let-expressions with their bodies.
   The list should be in the order Lets are encountered as you recurse down into
   the term, recursing first into in the left subexpression of an Add or Mult,
   and then into the right subexpression.

   Note that you do need to perform this replacement on e1 in an expression
   of the form Let x e1 e2.  This is a tiny bit tricky, so there is a second
   test for it in PublicTests ("extractLets 2").

   I have this set up using the Accum monad, which works very elegantly here.
-}
extractLets :: Expr -> Accum (Var,Expr) Expr
extractLets (Int x) = pure (Int x)
extractLets (Add e1 e2) = pure Add <*> extractLets e1 <*> extractLets e2
extractLets (Mult e1 e2) = pure Mult <*> extractLets e1 <*> extractLets e2
extractLets (Use x) = pure (Use x)
extractLets (Let x e1 e2) = 
   let (Accum l' e') = extractLets e1 in
      Accum (l' ++ [(x,e')]) id <*> extractLets e2 


{- given a list of pairs (v,e1) representing defining variable v to be e1,
   wrap those pairs around the given Expr e using Let-expressions. See
   the "wrapLets" test in PublicTests for an example. -}
wrapLets :: [(Var,Expr)] -> Expr -> Expr
wrapLets [] e = e
wrapLets ((v,e1):vs) e = Let v e1 (wrapLets vs e)

    
{- using extractLets and wrapLets, lift all Lets out to the top of an Expr.
   See the "liftLets" test in PublicTests for an example. -}
liftLets :: Expr -> Expr
liftLets e = let (Accum l' e') = extractLets e in 
   wrapLets l' e'

{- collect all the variables that appear anywhere in the given expression.
   The returned list may contain duplicates.  These are removed
   below. -}
collectVarsh :: Expr -> [Var]
collectVarsh e = let (Accum l' e') = extractLets e in
   [i | (i,j) <- l']

{- collect all the variables that appear anywhere in the given expression,
   with no duplicates -}
collectVars :: Expr -> [Var]
collectVars = canonOrd . collectVarsh 
