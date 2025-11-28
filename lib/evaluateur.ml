(* Environnements de renommation *)
type mapping = (string * string) list

(* générateur de noms frais de variables *)
let compteur_var : int ref = ref 0

let nouvelle_var () : string =
  compteur_var := !compteur_var + 1;
  "x" ^ string_of_int !compteur_var

let compteur_var_adr : Common.address ref = ref 0

let nouvelle_adresse () : Common.address =
  compteur_var_adr := !compteur_var_adr + 1;
  !compteur_var_adr

exception VarPasTrouve of string

(* cherche le nouveau nom d'une variable dans un mapping *)
let rec cherche_var (x : string) (map : mapping) : string =
  match map with
  | [] -> raise (VarPasTrouve x)
  | (x1, y1) :: _q when x1 = x -> y1
  | (_, _) :: q -> cherche_var x q

(* Changer les noms de variables pour assurer qu'il n'y a pas de répétition *)
let rec alpha_convert (t : Common.pterm) : Common.pterm =
  alpha_convert_helper t []

and alpha_convert_helper (t : Common.pterm) (map : mapping) : Common.pterm =
  match t with
  | Var x ->
      let x1 = cherche_var x map in
      Var x1
  | App (u, v) -> App (alpha_convert_helper u map, alpha_convert_helper v map)
  | Abs (x, u) ->
      let x1 = nouvelle_var () in
      Abs (x1, alpha_convert_helper u ((x, x1) :: map))
  | N i -> N i
  | Add (t1, t2) ->
      Add (alpha_convert_helper t1 map, alpha_convert_helper t2 map)
  | Sub (t1, t2) ->
      Sub (alpha_convert_helper t1 map, alpha_convert_helper t2 map)
  | Mult (t1, t2) ->
      Mult (alpha_convert_helper t1 map, alpha_convert_helper t2 map)
  | EmptyList -> EmptyList
  | Cons (hd, tl) ->
      Cons (alpha_convert_helper hd map, alpha_convert_helper tl map)
  | Head l -> Head (alpha_convert_helper l map)
  | Tail l -> Tail (alpha_convert_helper l map)
  | IfZero (c, t, f) ->
      IfZero
        ( alpha_convert_helper c map,
          alpha_convert_helper t map,
          alpha_convert_helper f map )
  | IfEmpty (c, t, f) ->
      IfEmpty
        ( alpha_convert_helper c map,
          alpha_convert_helper t map,
          alpha_convert_helper f map )
  | Let (x, e1, e2) ->
      let x1 = nouvelle_var () in
      Let
        ( x1,
          alpha_convert_helper e1 map,
          alpha_convert_helper e2 ((x, x1) :: map) )
  | Ref m -> Ref (alpha_convert_helper m map)
  | Deref m -> Deref (alpha_convert_helper m map)
  | Assign (e1, e2) ->
      Assign (alpha_convert_helper e1 map, alpha_convert_helper e2 map)
  | Fix (phi, m) ->
      let phi1 = nouvelle_var () in
      Fix (phi1, alpha_convert_helper m ((phi, phi1) :: map))
  | e -> e

(* Substitue une variable par un terme dans un autre terme *)
let rec substitue_var (t : Common.pterm) (x : string) (t0 : Common.pterm) :
    Common.pterm =
  match t with
  | Var x1 when x1 = x -> t0
  | Var v2 -> Var v2
  | App (u, v) -> App (substitue_var u x t0, substitue_var v x t0)
  | Abs (x1, u) when x1 = x ->
      Abs (x1, u)
      (* re-linking name, which means we can't reference x in u at all *)
  | Abs (y, u) -> Abs (y, substitue_var u x t0)
  | N i -> N i
  | Add (t1, t2) -> Add (substitue_var t1 x t0, substitue_var t2 x t0)
  | Sub (t1, t2) -> Sub (substitue_var t1 x t0, substitue_var t2 x t0)
  | Mult (t1, t2) -> Mult (substitue_var t1 x t0, substitue_var t2 x t0)
  | EmptyList -> EmptyList
  | Cons (hd, tl) -> Cons (substitue_var hd x t0, substitue_var tl x t0)
  | Head l -> Head (substitue_var l x t0)
  | Tail l -> Tail (substitue_var l x t0)
  | IfZero (c, t, f) ->
      IfZero (substitue_var c x t0, substitue_var t x t0, substitue_var f x t0)
  | IfEmpty (c, t, f) ->
      IfEmpty (substitue_var c x t0, substitue_var t x t0, substitue_var f x t0)
  | Let (y, e1, e2) -> Let (y, substitue_var e1 x t0, substitue_var e2 x t0)
  | Ref m -> Ref (substitue_var m x t0)
  | Deref m -> Deref (substitue_var m x t0)
  | Assign (e1, e2) -> Assign (substitue_var e1 x t0, substitue_var e2 x t0)
  | Fix (phi, m) -> Fix (phi, substitue_var m x t0)
  | e -> e

exception AppToNonAbs
exception Echec_typage of string
(* TODO: Is there some way to avoid this in the evaluator?
   In APS, we just said that we assume the typer was run before the evaluator. *)

type mem = (int, Common.pterm) Hashtbl.t

(* Evaluateur left-to-right, call-by-value *)
let rec eval_with_mem (t : Common.pterm) (sigma : mem) : (Common.pterm * mem) =
  (* print_endline ("eval_with_mem " ^ Common.print_term t); *)
  match t with
  | Var x -> Var x, sigma
  | Abs (x, u) -> Abs (x, u), sigma (* on ne réduit pas sous les lambdas *)
  | App (m, n) -> (
      let (m_val, m_mem) = eval_with_mem m sigma in
      let (n_val, n_mem) = eval_with_mem n m_mem in
      (* print_endline ("m_val: " ^ Common.print_term m_val);
      print_endline ("n_val: " ^ Common.print_term n_val); *)
      match m_val with
      | Abs (x, m_prime) -> eval_with_mem (substitue_var m_prime x n_val) sigma
      | e -> e, n_mem
      (* | _ -> raise AppToNonAbs *))
  | N i -> N i, sigma
  | Add (t1, t2) -> (
      let (a, a_mem) = eval_with_mem t1 sigma in
      let (b, b_mem) = eval_with_mem t2 a_mem in
      match (a, b) with
      | N n1, N n2 -> N (n1 + n2), b_mem
      | t3, t4 ->
          Add (t3, t4), b_mem
          (* | (r1, r2) -> raise (Echec_typage ("typing error on + applied to " ^ (Common.print_term r1) ^ " and " ^ (Common.print_term r2)))) *)
      )
  | Sub (t1, t2) -> (
      let (a, a_mem) = eval_with_mem t1 sigma in
      let (b, b_mem) = eval_with_mem t2 a_mem in
      match (a, b) with
      | N n1, N n2 -> N (n1 - n2), b_mem
      | t3, t4 ->
          Sub (t3, t4), b_mem
          (* | (r1, r2) -> raise (Echec_typage ("typing error on - applied to " ^ (Common.print_term r1) ^ " and " ^ (Common.print_term r2)))) *)
      )
  | Mult (t1, t2) -> (
      let (a, a_mem) = eval_with_mem t1 sigma in
      let (b, b_mem) = eval_with_mem t2 a_mem in
      match (a, b) with
      | N n1, N n2 -> N (n1 * n2), b_mem
      | t3, t4 ->
          Mult (t3, t4), b_mem
          (* | (r1, r2) -> raise (Echec_typage ("typing error on * applied to " ^ (Common.print_term r1) ^ " and " ^ (Common.print_term r2)))) *)
      )
  | EmptyList -> EmptyList, sigma
  | Cons (hd, tl) ->
      let (h, h_mem) = eval_with_mem hd sigma in
      let (t, t_mem) = eval_with_mem tl h_mem in
      Cons (h, t), t_mem
  | Head l -> (
      match eval_with_mem l sigma with
      | Cons (hd, _tl), sigma_prime -> hd, sigma_prime
      | t ->
          t
          (* | _ -> raise (Echec_typage ("typing error on head applied to " ^ (Common.print_term l)))) *)
      )
  | Tail l -> (
      match eval_with_mem l sigma with
      | Cons (_hd, tl), sigma_prime -> tl, sigma_prime
      | t ->
          t
          (* | _ -> raise (Echec_typage ("typing error on tail applied to " ^ (Common.print_term l)))) *)
      )
  | IfZero (c, t, f) -> (
      let (cond, cond_mem) = eval_with_mem c sigma in
      match cond with
      | N 0 -> eval_with_mem t cond_mem
      | _ -> eval_with_mem f cond_mem)
  | IfEmpty (c, t, f) -> (
      let (cond, cond_mem) = eval_with_mem c sigma in
      match cond with
      | EmptyList -> eval_with_mem t cond_mem
      | _ -> eval_with_mem f cond_mem)
  | Let (x, e1, e2) ->
      let t1, sigma_prime = eval_with_mem e1 sigma in
      let s2 = substitue_var e2 x t1 in
      eval_with_mem s2 sigma_prime
  | Ref e ->
      let adr = nouvelle_adresse () in
      let (r, r_mem) = eval_with_mem e sigma in
      Hashtbl.add r_mem adr (r);
      Address adr, r_mem
  | Deref e -> (
      let (d, d_mem) = eval_with_mem e sigma in
      match d with
      | Address adr -> (Hashtbl.find sigma adr), d_mem
      | t ->
          failwith
            ("Attempted to dereference non-address: " ^ Common.print_term t))
  | Assign (e1, e2) -> (
      let t1, sigma_prime = eval_with_mem e1 sigma in
      match t1 with
      | Address adr ->
          let t2, sigma_pprime = eval_with_mem e2 sigma_prime in
          Hashtbl.replace sigma_pprime adr t2;
          Unit, sigma_pprime
      | t ->
          failwith ("Attempted to assign to non-address: " ^ Common.print_term t)
      )
  | Fix (phi, m) -> substitue_var m phi (Fix (phi, m)), sigma
  | Address adr -> Address adr, sigma
  | Unit -> Unit, sigma

let eval (t : Common.pterm) =
  compteur_var := 0;
  compteur_var_adr := 0;
  let t1 = alpha_convert t in
  let sigma : mem = Hashtbl.create 123 in
  let (result, _memory) = eval_with_mem t1 sigma in
  result
