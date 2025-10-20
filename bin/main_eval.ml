open Typeur_evaluateur

(* ***EXEMPLES*** *)
let ex_id : Evaluateur.pterm = Abs ("x", Var "x")
let conv_ex_id = Evaluateur.alpha_convert ex_id
let ex_k : Evaluateur.pterm = Abs ("x", Abs ("y", Var "x"))
let conv_ex_k = Evaluateur.alpha_convert ex_k
let ex_s : Evaluateur.pterm = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let conv_ex_s = Evaluateur.alpha_convert ex_s
let ex_relink : Evaluateur.pterm = Abs ("x", Abs ("y", App (Var "x", Abs ("x", App (Var "x", Var "y")))))
let conv_ex_relink = Evaluateur.alpha_convert ex_relink
let ex_sub : Evaluateur.pterm = App (Var "x", Abs ("x", Var "x"))
let sub_ex_sub = Evaluateur.substitue_var ex_sub "x" (Abs ("y", App (Var "y", Var "y")))

let main () =
  print_endline "Alpha-conversion";
  print_endline "======================";
  print_endline (Evaluateur.print_term conv_ex_id);
  print_endline "======================";
  print_endline (Evaluateur.print_term conv_ex_k);
  print_endline "======================";
  print_endline (Evaluateur.print_term conv_ex_s);
  print_endline "======================";
  print_endline (Evaluateur.print_term conv_ex_relink);
  print_endline "";
  print_endline "Substitution";
  print_endline "======================";
  print_endline (Evaluateur.print_term sub_ex_sub)

let _ = main ()
