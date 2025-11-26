open Typeur_evaluateur

let () =
  let fname = Sys.argv.(1) in
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
    print_string (Common.print_term p ^ " => ");
    let result = Evaluateur.eval p in
    print_endline (Common.print_term result)
  with Lexer.Eof ->
    exit 0
