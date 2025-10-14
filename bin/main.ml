open Typeur_evaluateur

(* ***EXEMPLES*** *)
let ex_id : Minitypeur.pterm = Abs ("x", Var "x")
let inf_ex_id : string = Minitypeur.inference ex_id
let ex_k : Minitypeur.pterm = Abs ("x", Abs ("y", Var "x"))
let inf_ex_k : string = Minitypeur.inference ex_k
let ex_s : Minitypeur.pterm = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let inf_ex_s : string = Minitypeur.inference ex_s
let ex_nat1 : Minitypeur.pterm = App (Abs ("x", Add(Var "x", N 1)), N 3)
let inf_ex_nat1 : string = Minitypeur.inference ex_nat1
let ex_nat2 : Minitypeur.pterm = Abs ("x", Add( Var "x", Var "x"))
let inf_ex_nat2 : string = Minitypeur.inference ex_nat2
let ex_omega : Minitypeur.pterm = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))
let inf_ex_omega : string = Minitypeur.inference ex_omega
let ex_nat3 : Minitypeur.pterm = App (ex_nat2, ex_id)
let inf_ex_nat3 : string = Minitypeur.inference ex_nat3

let main () =
  print_endline "======================";
  print_endline inf_ex_id;
  print_endline "======================";
  print_endline inf_ex_k;
  print_endline "======================";
  print_endline inf_ex_s;
  print_endline "======================";
  print_endline inf_ex_omega;
  print_endline "======================";
  print_endline inf_ex_nat1;
  print_endline "======================";
  print_endline inf_ex_nat2;
  print_endline "======================";
  print_endline inf_ex_nat3

let _ = main ()
