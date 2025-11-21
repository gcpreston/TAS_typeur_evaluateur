open OUnit2
open Typeur_evaluateur

let inference_test_failure (res : Typeur.inference_result) =
  assert_bool (Typeur.print_inference_result res) false

let ex_id : Common.pterm = Abs ("x", Var "x")
let ex_k : Common.pterm = Abs ("x", Abs ("y", Var "x"))
let ex_nat1 : Common.pterm = App (Abs ("x", Add (Var "x", N 1)), N 3)
let ex_nat2 : Common.pterm = Abs ("x", Add (Var "x", Var "x"))

let test_id _ =
  match Typeur.inference ex_id with
  | Typable (ArrowType (VarType a, VarType b)) when a = b -> ()
  | res -> inference_test_failure res

let test_k _ =
  match Typeur.inference ex_k with
  | Typable (ArrowType (a, ArrowType (b, c))) when a = c && a != b -> ()
  | res -> inference_test_failure res

let test_nat1 _ =
  match Typeur.inference ex_nat1 with
  | Typable NatType -> ()
  | res -> inference_test_failure res

let test_nat2 _ =
  match Typeur.inference ex_nat2 with
  | Typable (ArrowType (NatType, NatType)) -> ()
  | res -> inference_test_failure res

(* >:: names a test. Then we put them in named groups, for organization with
   >:::. *)
let suite =
  "TypeurTests"
  >::: [
         "test_id" >:: test_id;
         "test_k" >:: test_k;
         "test_nat1" >:: test_nat1;
         "test_nat2" >:: test_nat2;
       ]

(* Run the test suite. *)
let () = run_test_tt_main suite
