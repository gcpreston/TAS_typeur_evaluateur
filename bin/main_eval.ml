open Typeur_evaluateur

(* ***EXEMPLES*** *)
let ex_id : Common.pterm = Abs ("x", Var "x")
let conv_ex_id = Evaluateur.alpha_convert ex_id
let ex_k : Common.pterm = Abs ("x", Abs ("y", Var "x"))
let conv_ex_k = Evaluateur.alpha_convert ex_k
let ex_s : Common.pterm = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let conv_ex_s = Evaluateur.alpha_convert ex_s
let ex_relink : Common.pterm = Abs ("x", Abs ("y", App (Var "x", Abs ("x", App (Var "x", Var "y")))))
let conv_ex_relink = Evaluateur.alpha_convert ex_relink
let ex_sub : Common.pterm = App (Var "x", Abs ("x", Var "x"))
let sub_ex_sub = Evaluateur.substitue_var ex_sub "x" (Abs ("y", App (Var "y", Var "y")))
let ex_id_k : Common.pterm = App (ex_id, ex_k)
let eval_ex_id_k = Evaluateur.eval ex_id_k
(* let ex_add : Common.pterm = Abs ("n", Abs ("m", Abs ("f", Abs ("e", App(Var "n", App (Var "f", App (Var "m", App (Var "f", Var "e"))))))))
let ex_mult : Common.pterm = Abs ("n", Abs ("m", Abs ("f", Abs ("e", App(App (Var "n", App (Var "m", Var "f")), Var "e")))))
let ex_2 : Common.pterm = Abs ("f", Abs ("e", App (Var "f", App (Var "f", Var "e"))))
let ex_3 : Common.pterm = Abs ("f", Abs ("e", App (Var "f", App (Var "f", App (Var "f", Var "e")))))
let ex_add_2_3 : Common.pterm = App (App (ex_add, ex_2), ex_3)
let eval_ex_add_2_3 = Evaluateur.eval ex_add_2_3 *)
(* let ex_delta : Common.pterm = Abs ("x", App (Var "x", Var "x"))
let ex_delta_delta : Common.pterm = App (ex_delta, ex_delta)
let eval_ex_delta_delta = Evaluateur.eval ex_delta_delta *)
let ex_id_id_x : Common.pterm = App (ex_id, App(ex_id, Var "x"))
let eval_ex_id_id_x = Evaluateur.eval ex_id_id_x
let ex_let : Common.pterm = Let ("x", Add (N 1, N 2), Add (Var "x", N 10))
let eval_ex_let = Evaluateur.eval ex_let
(* let ex_rec_add : Common.pterm = App (App (
  Fix (
    Abs ("a", Abs ("b",
      IfZero (
        Var "a",
        Var "b",
        (Sub (App "phi" (Add (Var "a", N 1), Var "b")), N 1)
      )
    ))
  )
), N 2), N 3 *)
let ex_rec : Common.pterm = App (
  Fix (Abs ("n", IfZero (Var "n", N 1234, App (Var "phi", Sub (Var "n", N 1))))),
  N 3
)
let eval_ex_rec : Common.pterm = Evaluateur.eval ex_rec

let main () =
  print_endline "Alpha-conversion";
  print_endline "======================";
  print_endline (Common.print_term conv_ex_id);
  print_endline "======================";
  print_endline (Common.print_term conv_ex_k);
  print_endline "======================";
  print_endline (Common.print_term conv_ex_s);
  print_endline "======================";
  print_endline (Common.print_term conv_ex_relink);
  print_endline "";
  print_endline "Substitution";
  print_endline "======================";
  print_endline (Common.print_term sub_ex_sub);
  print_endline "";
  print_endline "Evaluation";
  print_endline "======================";
  (* print_endline (Common.print_term eval_ex_add_2_3); *)
  print_endline (Common.print_term eval_ex_id_k);
  print_endline (Common.print_term eval_ex_id_id_x);
  print_endline (Common.print_term eval_ex_let);
  print_endline (Common.print_term eval_ex_rec)

let _ = main ()
