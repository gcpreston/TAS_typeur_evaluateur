open Typeur_evaluateur

let () =
  let fname = Sys.argv.(1) in
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
    print_endline (Typeur.print_inference_result (Typeur.inference p))
  with Lexer.Eof -> exit 0
