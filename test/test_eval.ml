open OUnit2
open Typeur_evaluateur

let eval_test_failure (res : Common.pterm) =
  assert_bool (Common.print_term res) false

let ex_id : Common.pterm = Abs ("x", Var "x")
let ex_id_id_5 : Common.pterm = App (ex_id, App (ex_id, N 5))
let ex_let : Common.pterm = Let ("x", Add (N 1, N 2), Add (Var "x", N 10))

let ex_fix : Common.pterm =
  App
    ( Fix
        ( "phi",
          Abs ("n", IfZero (Var "n", N 123, App (Var "phi", Sub (Var "n", N 1))))
        ),
      N 3 )

let ex_ref : Common.pterm = Let ("r", Ref EmptyList, Deref (Var "r"))

let ex_assign : Common.pterm =
  Let
    ( "l",
      Ref EmptyList,
      Let
        ( "_",
          Assign (Var "l", Cons (Abs ("x", Var "x"), EmptyList)),
          Add (Head (Deref (Var "l")), N 2) ) )

let assert_eval (term : Common.pterm) (expected : Common.pterm) =
  let result = Evaluateur.eval term in
  if result = expected then
    ()
  else
    eval_test_failure result

let eval_expectations : (string * Common.pterm * Common.pterm) list = [
  ("test_id", ex_id, Abs ("x1", Var "x1"));
  ("test_id_id_5", ex_id_id_5, N 5);
  ("test_let", ex_let, N 13);
  ("test_fix", ex_fix, N 123);
  ("test_ref", ex_ref, EmptyList);
  ("test_assign", ex_assign, Add (Abs ("x3", Var "x3"), N 2));
]

let expectation_to_test (tup : string * Common.pterm * Common.pterm) =
  let (name, term, expected) = tup in
  name >:: fun _ -> assert_eval term expected

let evaluateur_tests = List.map expectation_to_test eval_expectations

(* >:: names a test. Then we put them in named groups, for organization with
   >:::. *)

let suite = "EvaluateurTests" >::: evaluateur_tests

(* Run the test suite. *)
let () =
  run_test_tt_main suite
