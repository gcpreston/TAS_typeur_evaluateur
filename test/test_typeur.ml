open OUnit2
open Typeur_evaluateur

let inference_test_failure (res : Typeur.inference_result) =
  assert_bool (Typeur.print_inference_result res) false

let ex_id : Common.pterm = Abs ("x", Var "x")
let ex_k : Common.pterm = Abs ("x", Abs ("y", Var "x"))
let ex_nat1 : Common.pterm = App (Abs ("x", Add (Var "x", N 1)), N 3)
let ex_nat2 : Common.pterm = Abs ("x", Add (Var "x", Var "x"))

let ex_omega : Common.pterm =
  App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))

let ex_nat3 : Common.pterm = App (ex_nat2, ex_id)
let ex_lst1 : Common.pterm = EmptyList
let ex_hd : Common.pterm = Head (Cons (N 5, EmptyList))
let ex_tl : Common.pterm = Tail (Cons (N 5, EmptyList))
let ex_ifzero : Common.pterm = IfZero (N 1, EmptyList, EmptyList)
let ex_ifzero_fail : Common.pterm = IfZero (N 0, N 5, EmptyList)
let ex_ifempty : Common.pterm = IfEmpty (EmptyList, N 5, N 10)
let ex_let : Common.pterm = Let ("x", N 5, Add (Var "x", N 4))

let ex_let_poly : Common.pterm =
  Let
    ( "id",
      Abs ("x", Var "x"),
      Let ("_", App (Var "id", N 0), App (Var "id", EmptyList)) )

let ex_let_exact : Common.pterm = Let ("id", Abs ("x", Var "x"), App (Var "id", N 5))

let ex_let_fail : Common.pterm =
  Let
    ( "id",
      Abs ("x", Var "x"),
      IfEmpty (EmptyList, App (Var "id", N 5), App (Var "id", EmptyList)) )

let ex_ref : Common.pterm = Let ("x", Ref EmptyList, Cons (N 1, Deref (Var "x")))

let ex_assign_fail : Common.pterm =
  Let
    ( "l",
      Ref EmptyList,
      Let
        ( "_",
          Assign (Var "l", Cons (Abs ("x", Var "x"), EmptyList)),
          Add (Head (Deref (Var "l")), N 2) ) )

let ex_fix : Common.pterm =
  Fix
    ( "phi",
      Abs ("n", IfZero (Var "n", N 123, App (Var "phi", Sub (Var "n", N 1)))) )

let assert_type (term : Common.pterm) (expected : Typeur.inference_result) =
  let result = Typeur.inference term in
  if result = expected then () else inference_test_failure result

let type_inferences_expectations :
    (string * Common.pterm * Typeur.inference_result) list =
  [
    ("test_id", ex_id, Typable (ArrowType (VarType "T2", VarType "T2")));
    ( "test_k",
      ex_k,
      Typable (ArrowType (VarType "T4", ArrowType (VarType "T3", VarType "T4")))
    );
    ("test_nat1", ex_nat1, Typable NatType);
    ("test_nat2", ex_nat2, Typable (ArrowType (NatType, NatType)));
    ("test_omega", ex_omega, PasTypable "occurence de T5 dans (T5 -> T2)");
    ("test_nat3", ex_nat3, PasTypable "type entier non-unifiable avec (T5 -> T6)");
    ("test_lst1", ex_lst1, Typable (ListType (VarType "T1")));
    ("test_hd", ex_hd, Typable NatType);
    ("test_tl", ex_tl, Typable (ListType NatType));
    ("test_ifzero", ex_ifzero, Typable (ListType (VarType "T2")));
    ( "test_ifzero_fail",
      ex_ifzero_fail,
      PasTypable "type entier non-unifiable avec [T2]" );
    ("test_ifempty", ex_ifempty, Typable NatType);
    ("test_let", ex_let, Typable NatType);
    ("test_let_poly", ex_let_poly, Typable (ListType (VarType "T9")));
    ("test_let_exact", ex_let_exact, Typable NatType);
    ( "test_let_fail",
      ex_let_fail,
      PasTypable "type entier non-unifiable avec [T11]" );
    ("test_ref", ex_ref, Typable (ListType NatType));
    ( "test_assign_fail",
      ex_assign_fail,
      PasTypable "type entier non-unifiable avec (T8 -> T8)" );
    ("test_fix", ex_fix, Typable (ArrowType (NatType, NatType)));
  ]

let expectation_to_test (tup : string * Common.pterm * Typeur.inference_result)
    =
  let name, term, expected = tup in
  name >:: fun _ -> assert_type term expected

let typeur_tests = List.map expectation_to_test type_inferences_expectations

(* >:: names a test. Then we put them in named groups, for organization with
   >:::. *)

let suite = "TypeurTests" >::: typeur_tests

(* Run the test suite. *)
let () = run_test_tt_main suite
