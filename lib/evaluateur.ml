(* Environnements de renommation *)
type mapping = (string * string) list

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
let rec alpha_convert (t : Common.pterm) : Common.pterm = alpha_convert_helper t []
and
alpha_convert_helper (t : Common.pterm) (map : mapping) : Common.pterm =
  match t with
    Var x ->
      let x1 = cherche_var x map in
      Var x1
    | App (u, v) ->
      App ((alpha_convert_helper u map), (alpha_convert_helper v map))
    | Abs (x, u) ->
      let x1 = nouvelle_var () in
      Abs (x1, alpha_convert_helper u ((x, x1)::map))
    | N i -> N i
    | Add (t1, t2) -> Add (alpha_convert_helper t1 map, alpha_convert_helper t2 map)
    | Sub (t1, t2) -> Sub (alpha_convert_helper t1 map, alpha_convert_helper t2 map)
    | EmptyList -> EmptyList
    | Cons (hd, tl) -> Cons (alpha_convert_helper hd map, alpha_convert_helper tl map)
    | Head l -> Head (alpha_convert_helper l map)
    | Tail l -> Tail (alpha_convert_helper l map)
    | IfZero (c, t, f) -> IfZero (alpha_convert_helper c map, alpha_convert_helper t map, alpha_convert_helper f map)
    | IfEmpty (c, t, f) -> IfEmpty (alpha_convert_helper c map, alpha_convert_helper t map, alpha_convert_helper f map)
    | Let (x, e1, e2) -> Let (x, alpha_convert_helper e1 map, alpha_convert_helper e2 map)
    | Fix m -> Fix (alpha_convert_helper m map)

(* Substitue une variable par un terme dans un autre terme *)
let rec substitue_var (t : Common.pterm) (x : string) (t0 : Common.pterm) : Common.pterm =
  match t with
    Var x1 when x1 = x -> t0
    | Var v2 -> Var v2
    | App (u, v) -> App (substitue_var u x t0, substitue_var v x t0)
    | Abs (x1, u) when x1 = x -> Abs (x1, u) (* re-linking name, which means we can't reference x in u at all *)
    | Abs (y, u) -> Abs (y, substitue_var u x t0)
    | N i -> N i
    | Add (t1, t2) -> Add (substitue_var t1 x t0, substitue_var t2 x t0)
    | Sub (t1, t2) -> Sub (substitue_var t1 x t0, substitue_var t2 x t0)
    | EmptyList -> EmptyList
    | Cons (hd, tl) -> Cons (substitue_var hd x t0, substitue_var tl x t0)
    | Head l -> Head (substitue_var l x t0)
    | Tail l -> Tail (substitue_var l x t0)
    | IfZero (c, t, f) -> IfZero (substitue_var c x t0, substitue_var t x t0, substitue_var f x t0)
    | IfEmpty (c, t, f) -> IfEmpty (substitue_var c x t0, substitue_var t x t0, substitue_var f x t0)
    | Let (x, e1, e2) -> Let (x, substitue_var e1 x t0, substitue_var e2 x t0)
    | Fix m -> Fix m

exception AppToNonAbs
exception Echec_typage of string
(* TODO: Is there some way to avoid this in the evaluator?
   In APS, we just said that we assume the typer was run before the evaluator. *)

(* TODO: Have outer wrapper to call alpha_convert first *)

(* Evaluateur left-to-right, call-by-value *)
let rec eval (t : Common.pterm) : Common.pterm =
  print_endline ("eval " ^ (Common.print_term t));
  match t with
    Var x -> Var x
    | Abs (x, u) -> Abs (x, eval u)
    | App (m, n) ->
      let m_val = eval m in
      let n_val = eval n in
      (match m_val with
        Fix m -> let m_prime = eval (substitue_var m "phi" (Fix m)) in
          eval (App (m_prime, n_val))
        | Abs (x, m_prime) -> eval (substitue_var m_prime x n_val)
        | e -> e)
        (* | _ -> raise AppToNonAbs *)
    | N i -> N i
    | Add (t1, t2) ->
      (match (eval t1, eval t2) with
        (N n1, N n2) -> N (n1 + n2)
        | (t3, t4) -> Add (t3, t4))
        (* | (r1, r2) -> raise (Echec_typage ("typing error on + applied to " ^ (Common.print_term r1) ^ " and " ^ (Common.print_term r2)))) *)
    | Sub (t1, t2) ->
      (match (eval t1, eval t2) with
        (N n1, N n2) -> N (n1 - n2)
        | (t3, t4) -> Sub (t3, t4))
        (* | (r1, r2) -> raise (Echec_typage ("typing error on - applied to " ^ (Common.print_term r1) ^ " and " ^ (Common.print_term r2)))) *)
    | EmptyList -> EmptyList
    | Cons (hd, tl) -> Cons (eval hd, eval tl)
    | Head l ->
      (match eval l with
        Cons (hd, _tl) -> hd
        | t -> t)
        (* | _ -> raise (Echec_typage ("typing error on head applied to " ^ (Common.print_term l)))) *)
    | Tail l ->
      (match eval l with
        Cons (_hd, tl) -> tl
        | t -> t)
        (* | _ -> raise (Echec_typage ("typing error on tail applied to " ^ (Common.print_term l)))) *)
    | IfZero (c, t, f) ->
      (match eval c with
        N 0 -> eval t
        | _ -> eval f)
    | IfEmpty (c, t, f) ->
      (match eval c with
        EmptyList -> eval t
        | _ -> eval f)
    | Let (x, e1, e2) -> eval (App (Abs (x, e2), e1)) (* TODO: This feels like it wants to be more complex lol *)
    | Fix m -> Fix m
