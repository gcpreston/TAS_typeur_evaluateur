open Typeur_evaluateur

(* ***EXEMPLES*** *)
let ex_id : Evaluateur.pterm = Abs ("x", Var "x")
let inf_ex_id = Evaluateur.alpha_convert ex_id
let ex_k : Evaluateur.pterm = Abs ("x", Abs ("y", Var "x"))
let inf_ex_k = Evaluateur.alpha_convert ex_k
let ex_s : Evaluateur.pterm = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let inf_ex_s = Evaluateur.alpha_convert ex_s
let ex_relink : Evaluateur.pterm = Abs ("x", Abs ("y", App (Var "x", Abs ("x", App (Var "x", Var "y")))))
let inf_ex_relink = Evaluateur.alpha_convert ex_relink

let main () =
  print_endline "======================";
  print_endline (Evaluateur.print_term inf_ex_id);
  print_endline "======================";
  print_endline (Evaluateur.print_term inf_ex_k);
  print_endline "======================";
  print_endline (Evaluateur.print_term inf_ex_s);
  print_endline "======================";
  print_endline (Evaluateur.print_term inf_ex_relink)

let _ = main ()
