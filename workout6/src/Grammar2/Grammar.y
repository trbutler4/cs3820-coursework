{
module Grammar where
import Tokens
import Prog
}

%name parseProg
%tokentype { Token }
%error { parseError }

%token
    '('  { TokenLParen }
    ')'  { TokenRParen }
    ';'  { TokenSemi}
    ','  { TokenComma}
    id  { TokenId $$ }
%%

-- replace with your productions:
Prog : FunCall ';' { [$1] } -- case of one FunCall
| FunCall ';' Prog { $1:$3 } -- case of more than one FunCall

FunCall : id { Var $1 }
| id '(' ')' { FunCall $1 [] }
| id '(' FunList ')' { FunCall $1 $3 }

FunList : FunCall { [$1] } 
| FunCall ',' FunList { $1 : $3 }

{

parseError :: [Token] -> a
parseError tks = error ("Parse error: " ++ show tks)

}
