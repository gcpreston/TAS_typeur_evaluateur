{
  open Parser
  exception Eof
}

rule token = parse
    [' ' '\t' '\n']  { token lexbuf }     (* skip blanks *)
  | '('              { LPAR }
  | ')'              { RPAR }
  | '+'              { PLUS }
  | '-'              { MINUS }
  | '*'              { TIMES }
  | "->"             { ARROW }
  | "fun"            { FUN }
  | "fix"            { FIX }
  | "ife"            { IFEMPTY }
  | "ifz"            { IFZERO }
  | "[]"             { EMPTYLIST }
  | "cons"           { CONS }
  | "hd"             { HEAD }
  | "tl"             { TAIL }
  | "let"            { LET }
  | "="              { EQUAL }
  | "in"             { IN }
  | "ref"            { REF }
  | "!"              { DEREF }
  | ":="             { ASSIGN }

  | ('-')?['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | ['a'-'z''_']['a'-'z''A'-'Z''0'-'9''_']* as lxm { IDENT(lxm) }

  | eof              { EOF }
