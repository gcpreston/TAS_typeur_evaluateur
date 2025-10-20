(* Termes *)
type pterm = Var of string | App of pterm * pterm | Abs of string * pterm

(* Environnements de renommation *)
type mapping = (string * string) list

(* pretty printer de termes*)
let rec print_term (t : pterm) : string =
  match t with
    Var x -> x
    | App (t1, t2) -> "(" ^ (print_term t1) ^" "^ (print_term t2) ^ ")"
    | Abs (x, t) -> "(fun "^ x ^" -> " ^ (print_term t) ^")"

(* générateur de noms frais de variables *)
let compteur_var : int ref = ref 0

let nouvelle_var () : string =
  compteur_var := !compteur_var + 1;
  "x" ^ (string_of_int !compteur_var)

exception VarPasTrouve

(* cherche le nouveau nom d'une variable dans un mapping *)
let rec cherche_var (x : string) (map : mapping) : string =
  match map with
    [] -> raise VarPasTrouve
  | (x1, y1)::_q when x1 = x -> y1
  | (_, _):: q -> (cherche_var x q)

(* Changer les noms de variables pour assurer qu'il n'y a pas de répétition *)
let rec alpha_convert (t : pterm) : pterm = alpha_convert_helper t []
and
alpha_convert_helper (t : pterm) (map : mapping) : pterm =
  match t with
    Var x ->
      let x1 = cherche_var x map in
      Var x1
    | App (u, v) ->
      App ((alpha_convert_helper u map), (alpha_convert_helper v map))
    | Abs (x, u) ->
      let x1 = nouvelle_var () in
      Abs (x1, alpha_convert_helper u ((x, x1)::map))

(* Substitue une variable par un terme dans un autre terme *)
let rec substitue_var (t : pterm) (x : string) (t0 : pterm) : pterm =
  match t with
    Var x1 when x1 = x -> t0
    | Var v2 -> Var v2
    | App (u, v) -> App (substitue_var u x t0, substitue_var v x t0)
    | Abs (x1, u) when x1 = x -> Abs (x1, u) (* re-linking name, which means we can't reference x in u at all *)
    | Abs (y, u) -> Abs (y, substitue_var u x t0)
