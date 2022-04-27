{
module Tokens where
}

%wrapper "basic"

tokens :-

  ab*                         { TokenIt "p0" }

  -- add further regular expressions here:

  c*d+                              { TokenIt "p1" }
  (e|f)+                            { TokenIt "p2" }
  [a-z]([a-z]|[0-9])+               { TokenIt "p3" }    
  (\"[a-zA-Z\ ]*\")|('[a-zA-Z\ ]*') { TokenIt "p4" }
  Y+ | Y*(XY*XY*)+                  { TokenIt "p5" }

  
  $white+                       ;
{
-- The token type:
data Token = TokenIt String {- which problem the string is for -} String
           deriving (Eq)

instance Show Token where
  show (TokenIt name str) = name ++ "(" ++ show str ++ ")"
}
