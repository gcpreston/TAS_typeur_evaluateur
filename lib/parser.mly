%{
  open Common
%}

(* APS0 *)
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR
%token PLUS MINUS TIMES
%token FUN ARROW
%token FIX
%token IFEMPTY IFZERO
%token EMPTYLIST CONS
%token HEAD TAIL
%token LET EQUAL IN
%token REF DEREF
%token ASSIGN
%token EOF

%type <Common.pterm> expr
%type <Common.pterm> prog

%start prog

%%
prog: expr EOF                { $1 }
;

expr:
  NUM                         { N $1 }
| IDENT                       { Var $1 }
| LPAR expr RPAR              { $2 }
| expr expr                   { App ($1, $2) }
| FUN IDENT ARROW expr        { Abs($2, $4) }
| IFEMPTY expr expr expr      { IfEmpty ($2, $3, $4) }
| IFZERO expr expr expr       { IfZero ($2, $3, $4) }
| expr PLUS expr              { Add ($1, $3) }
| expr MINUS expr             { Sub ($1, $3) }
| expr TIMES expr             { Mult ($1, $3) }
| EMPTYLIST                   { EmptyList }
| CONS expr expr              { Cons ($2, $3) }
| HEAD expr                   { Head $2 }
| TAIL expr                   { Tail $2 }
| LET IDENT EQUAL expr IN expr { Let ($2, $4, $6) }
| REF expr                    { Ref $2 }
| DEREF expr                  { Deref $2 }
| expr ASSIGN expr            { Assign ($1, $3) }
| FIX IDENT ARROW expr        { Fix ($2, $4) }
;
