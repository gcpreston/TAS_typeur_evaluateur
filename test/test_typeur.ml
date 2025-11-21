open OUnit2
open Typeur_evaluateur

let ex_id : Common.pterm = Abs ("x", Var "x")

(* A simple passing test *)
let test_id _ =
  match Typeur.inference ex_id with
    Typable (ArrowType (VarType _a, NatType)) -> assert_bool "test" true
    | _ -> assert_bool "test" false

  (* assert_equal (Typeur.inference ex_id) (Typable (ArrowType (VarType "T2", VarType "T2"))) *)

(* >:: names a test. Then we put them in named groups, for organization with
   >:::. *)
let suite =
  "ExampleTests" >:::
    ["test_id" >:: test_id]

(* Run the test suite. *)
let () =
  run_test_tt_main suite
