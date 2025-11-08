open Typeur_evaluateur

(* ***EXEMPLES*** *)
let ex_id : Typeur.pterm = Abs ("x", Var "x")
let inf_ex_id : string = Typeur.inference ex_id
let ex_k : Typeur.pterm = Abs ("x", Abs ("y", Var "x"))
let inf_ex_k : string = Typeur.inference ex_k
let ex_s : Typeur.pterm = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let inf_ex_s : string = Typeur.inference ex_s
let ex_nat1 : Typeur.pterm = App (Abs ("x", Add(Var "x", N 1)), N 3)
let inf_ex_nat1 : string = Typeur.inference ex_nat1
let ex_nat2 : Typeur.pterm = Abs ("x", Add( Var "x", Var "x"))
let inf_ex_nat2 : string = Typeur.inference ex_nat2
let ex_omega : Typeur.pterm = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))
let inf_ex_omega : string = Typeur.inference ex_omega
let ex_nat3 : Typeur.pterm = App (ex_nat2, ex_id)
let inf_ex_nat3 : string = Typeur.inference ex_nat3
let ex_lst1 : Typeur.pterm = EmptyList
let inf_ex_lst1 : string = Typeur.inference ex_lst1
let ex_lst2 : Typeur.pterm = Cons (N 5, EmptyList)
let inf_ex_lst2 : string = Typeur.inference ex_lst2
let ex_hd : Typeur.pterm = Head (Cons (N 5, EmptyList))
let inf_ex_hd : string = Typeur.inference ex_hd
let ex_tl : Typeur.pterm = Tail (Cons (N 5, EmptyList))
let inf_ex_tl : string = Typeur.inference ex_tl
let ex_ifzero : Typeur.pterm = IfZero (N 0, N 5, EmptyList)
let inf_ex_ifzero : string = Typeur.inference ex_ifzero
let ex_ifempty : Typeur.pterm = IfEmpty (EmptyList, N 5, N 10)
let inf_ex_ifempty : string = Typeur.inference ex_ifempty
let ex_let : Typeur.pterm = Let ("x", N 5, Add (Var "x", N 4))
let inf_ex_let : string = Typeur.inference ex_let

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
  print_endline inf_ex_nat3;
  print_endline "======================";
  print_endline inf_ex_lst1;
  print_endline "======================";
  print_endline inf_ex_lst2;
  print_endline "======================";
  print_endline inf_ex_hd;
  print_endline "======================";
  print_endline inf_ex_tl;
  print_endline "======================";
  print_endline inf_ex_ifzero;
  print_endline "======================";
  print_endline inf_ex_ifempty;
  print_endline "======================";
  print_endline inf_ex_let

let _ = main ()
