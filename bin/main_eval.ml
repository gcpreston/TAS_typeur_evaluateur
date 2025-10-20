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
let ex_id_k : Evaluateur.pterm = App (ex_id, ex_k)
let eval_ex_id_k = Evaluateur.eval ex_id_k
(* let ex_add : Evaluateur.pterm = Abs ("n", Abs ("m", Abs ("f", Abs ("e", App(Var "n", App (Var "f", App (Var "m", App (Var "f", Var "e"))))))))
let ex_mult : Evaluateur.pterm = Abs ("n", Abs ("m", Abs ("f", Abs ("e", App(App (Var "n", App (Var "m", Var "f")), Var "e")))))
let ex_2 : Evaluateur.pterm = Abs ("f", Abs ("e", App (Var "f", App (Var "f", Var "e"))))
let ex_3 : Evaluateur.pterm = Abs ("f", Abs ("e", App (Var "f", App (Var "f", App (Var "f", Var "e")))))
let ex_add_2_3 : Evaluateur.pterm = App (App (ex_mult, ex_2), ex_3)
let eval_ex_add_2_3 = Evaluateur.eval ex_add_2_3 *)
(* let ex_delta : Evaluateur.pterm = Abs ("x", App (Var "x", Var "x"))
let ex_delta_delta : Evaluateur.pterm = App (ex_delta, ex_delta)
let eval_ex_delta_delta = Evaluateur.eval ex_delta_delta *)
let ex_id_id_x : Evaluateur.pterm = App (ex_id, App(ex_id, Var "x"))
let eval_ex_id_id_x = Evaluateur.eval ex_id_id_x

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
  print_endline (Evaluateur.print_term sub_ex_sub);
  print_endline "";
  print_endline "Evaluation";
  print_endline "======================";
  print_endline (Evaluateur.print_term eval_ex_id_k);
  print_endline (Evaluateur.print_term eval_ex_id_id_x)

let _ = main ()
