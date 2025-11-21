open Typeur_evaluateur

(* ***EXEMPLES*** *)
let ex_id : Common.pterm = Abs ("x", Var "x")
let inf_ex_id : string = Typeur.inference ex_id
let ex_k : Common.pterm = Abs ("x", Abs ("y", Var "x"))
let inf_ex_k : string = Typeur.inference ex_k
let ex_s : Common.pterm = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let inf_ex_s : string = Typeur.inference ex_s
let ex_nat1 : Common.pterm = App (Abs ("x", Add(Var "x", N 1)), N 3)
let inf_ex_nat1 : string = Typeur.inference ex_nat1
let ex_nat2 : Common.pterm = Abs ("x", Add( Var "x", Var "x"))
let inf_ex_nat2 : string = Typeur.inference ex_nat2
let ex_omega : Common.pterm = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))
let inf_ex_omega : string = Typeur.inference ex_omega
let ex_nat3 : Common.pterm = App (ex_nat2, ex_id)
let inf_ex_nat3 : string = Typeur.inference ex_nat3
let ex_lst1 : Common.pterm = EmptyList
let inf_ex_lst1 : string = Typeur.inference ex_lst1
let ex_lst2 : Common.pterm = Cons (N 5, EmptyList)
let inf_ex_lst2 : string = Typeur.inference ex_lst2
let ex_hd : Common.pterm = Head (Cons (N 5, EmptyList))
let inf_ex_hd : string = Typeur.inference ex_hd
let ex_tl : Common.pterm = Tail (Cons (N 5, EmptyList))
let inf_ex_tl : string = Typeur.inference ex_tl
let ex_ifzero : Common.pterm = IfZero (N 0, N 5, EmptyList)
let inf_ex_ifzero : string = Typeur.inference ex_ifzero
let ex_ifempty : Common.pterm = IfEmpty (EmptyList, N 5, N 10)
let inf_ex_ifempty : string = Typeur.inference ex_ifempty
let ex_let : Common.pterm = Let ("x", N 5, Add (Var "x", N 4))
let inf_ex_let : string = Typeur.inference ex_let
let ex_let_fail : Common.pterm = Let ("id", Abs ("x", Var "x"), IfEmpty (EmptyList, App (Var "id", N 5), App (Var "id", EmptyList)))
let inf_ex_let_fail : string = Typeur.inference ex_let_fail
let ex_ref : Common.pterm = Let ("x", Ref EmptyList, Cons (N 1, Deref (Var "x")))
let inf_ex_ref : string = Typeur.inference ex_ref
let ex_assign : Common.pterm = Let ("l", Ref EmptyList, Let ("_", Assign (Var "l", Cons (Abs ("x", Var "x"), EmptyList)), Add (Head (Deref (Var "l")), N 2)))
let inf_ex_assign : string = Typeur.inference ex_assign
let ex_fix : Common.pterm = Fix ("phi", Abs ("n", IfZero (Var "n", N 123, App (Var "phi", Sub (Var "n", N 1)))))
let inf_ex_fix : string = Typeur.inference ex_fix

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
  print_endline inf_ex_let;
  print_endline "======================";
  print_endline inf_ex_let_fail;
  print_endline "======================";
  print_endline inf_ex_ref;
  print_endline "======================";
  print_endline inf_ex_assign;
  print_endline "======================";
  print_endline inf_ex_fix

let _ = main ()
