(* Types *)
type ptype = VarType of string
  | ArrowType of ptype * ptype
  | NatType
  | ListType of ptype
  | Unit
  | RefType of ptype
  | SchemeType of string list * ptype

(* Environnements de typage *)
type env = (string * ptype) list

(* Listes d'équations *)
type equa = (ptype * ptype) list

(* pretty printer de types*)
let rec print_type (t : ptype) : string =
  match t with
    VarType x -> x
  | ArrowType (t1, t2) -> "(" ^ (print_type t1) ^" -> "^ (print_type t2) ^")"
  | NatType -> "NatType"
  | ListType lt -> "[" ^ (print_type lt) ^ "]"
  | Unit -> "()"
  | RefType t -> "(ref " ^ (print_type t) ^ ")"
  | SchemeType (vars, e) -> (print_vars vars) ^ " . " ^ (print_type e)
and print_vars (vars : string list) : string =
  match vars with
    [] -> ""
  | v::[] -> v
  | v::rest -> v ^ ", " ^ (print_vars rest)

(* générateur de noms frais de variables de types *)
let compteur_var : int ref = ref 0

let nouvelle_var () : string = compteur_var := !compteur_var + 1;
  "T"^(string_of_int !compteur_var)


exception VarPasTrouve

(* cherche le type d'une variable dans un environnement *)
let rec cherche_type (v : string) (e : env) : ptype =
  match e with
    [] -> raise VarPasTrouve
  | (v1, t1)::_q when v1 = v -> t1
  | (_, _):: q -> (cherche_type v q)

(* vérificateur d'occurence de variables *)
let rec appartient_type (v : string) (t : ptype) : bool =
  match t with
    VarType v1 when v1 = v -> true
  | ArrowType (t1, t2) -> (appartient_type v t1) || (appartient_type v t2)
  | ListType t1 -> appartient_type v t1
  | RefType t1 -> appartient_type v t1
  | _ -> false

(* remplace une variable par un type dans type *)
let rec substitue_type (t : ptype) (v : string) (t0 : ptype) : ptype =
  match t with
    VarType v1 when v1 = v -> t0
  | VarType v2 -> VarType v2
  | ArrowType (t1, t2) -> ArrowType (substitue_type t1 v t0, substitue_type t2 v t0)
  | NatType -> NatType
  | ListType lt -> ListType (substitue_type lt v t0)
  | RefType t1 -> RefType (substitue_type t1 v t0)
  | SchemeType (vars, e) -> SchemeType (vars, substitue_type e v t0)
  | Unit -> Unit

(* remplace une variable par un type dans une liste d'équations*)
let substitue_type_partout (e : equa) (v : string) (t0 : ptype) : equa =
  List.map (fun (x, y) -> (substitue_type x v t0, substitue_type y v t0)) e

(* WISHLIST
 https://courses.cs.cornell.edu/cs3110/2021sp/textbook/interp/letpoly.html
 - generalize(context, environment, variable, type) -> scheme
 - instantiate(scheme) -> type
*)

let rec variables_libres_helper (t : ptype) (acc : string list) : string list =
  match t with
    VarType v -> v::acc
  | ArrowType (t1, t2) -> (variables_libres_helper t1 acc) @ (variables_libres_helper t2 acc)
  | ListType lt -> variables_libres_helper lt acc
  | RefType t1 -> variables_libres_helper t1 acc
  | _ -> acc

let variables_libres (t : ptype) : string list =
  let all_vars = variables_libres_helper t [] in
  List.sort_uniq compare all_vars

let rec instantiate (scheme : ptype) : ptype =
  match scheme with
    SchemeType ([], t) ->
      t
  | SchemeType (tv_head::tv_rest, t) ->
      let nv : string = nouvelle_var () in
      let substituted = substitue_type t tv_head (VarType nv) in
      instantiate (SchemeType (tv_rest, substituted))
  | _ -> scheme

let generalize (t : ptype) : ptype =
  match t with
    SchemeType (_, _) ->
      t
  | _ ->
      let vars_libres = variables_libres t in
      SchemeType (vars_libres, t)

(* genere des equations de typage à partir d'un terme *)
let rec genere_equa (te : Common.pterm) (ty : ptype) (e : env) : equa =
  match te with
    Var v -> let tv : ptype = instantiate (cherche_type v e) in [(ty, tv)]
  | App (t1, t2) -> let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (ArrowType (VarType nv, ty)) e in
      let eq2 : equa = genere_equa t2 (VarType nv) e in
      eq1 @ eq2
  | Abs (x, t) -> let nv1 : string = nouvelle_var ()
      and nv2 : string = nouvelle_var () in
      (ty, ArrowType (VarType nv1, VarType nv2))::(genere_equa t (VarType nv2) ((x, VarType nv1)::e))
  | N _ -> [(ty, NatType)]
  | Add (t1, t2) -> let eq1 : equa = genere_equa t1 NatType e in
      let eq2 : equa = genere_equa t2 NatType e in
      (ty, NatType)::(eq1 @ eq2)
  | Sub (t1, t2) -> let eq1 : equa = genere_equa t1 NatType e in
      let eq2 : equa = genere_equa t2 NatType e in
      (ty, NatType)::(eq1 @ eq2)
  | EmptyList -> let nv : string = nouvelle_var () in
      [(ty, ListType (VarType nv))]
  (* hd = T1, tl = [T1] *)
  | Cons (hd, tl) -> let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa hd (VarType nv) e in
      let eq2 : equa = genere_equa tl (ListType (VarType nv)) e in
      (ty, ListType (VarType nv))::(eq1 @ eq2)
  | Head l -> let nv : string = nouvelle_var () in
      (ty, VarType nv)::(genere_equa l (ListType (VarType nv)) e)
  | Tail l -> let nv : string = nouvelle_var () in
      (ty, ListType (VarType nv))::(genere_equa l (ListType (VarType nv)) e)
  | IfZero (c, t, f) -> let nv : string = nouvelle_var () in
      let eqc : equa = genere_equa c NatType e in
      let eqt : equa = genere_equa t (VarType nv) e in
      let eqf : equa = genere_equa f (VarType nv) e in
      (ty, (VarType nv))::(eqc @ eqt @ eqf)
  | IfEmpty (c, t, f) -> let nv1 : string = nouvelle_var () in
      let nv2 : string = nouvelle_var () in
      let eqc : equa = genere_equa c (ListType (VarType nv1)) e in
      let eqt : equa = genere_equa t (VarType nv2) e in
      let eqf : equa = genere_equa f (VarType nv2) e in
      (ty, (VarType nv2))::(eqc @ eqt @ eqf)
  | Let (x, e1, e2) -> let nv1 : string = nouvelle_var () in
      let nv2 : string = nouvelle_var () in
      let eq1 : equa = genere_equa e1 (VarType nv1) e in
      let eq2 : equa = genere_equa e2 (VarType nv2) ((x, generalize(VarType nv1))::e) in
      (ty, (VarType nv2))::(eq1 @ eq2)
  | Ref m -> let nv : string = nouvelle_var () in
      (ty, RefType (VarType nv))::(genere_equa m (VarType nv) e)
  | Deref m -> let nv : string = nouvelle_var () in
      (ty, (VarType nv))::(genere_equa m (RefType (VarType nv)) e)
  | Assign (e1, e2) -> let nv1 : string = nouvelle_var () in
      (* let nv2 : string = nouvelle_var () in *)
      let eq1 : equa = genere_equa e1 (RefType (VarType nv1)) e in
      let eq2 : equa = genere_equa e2 (VarType nv1) e in
      (ty, Unit)::(eq1 @ eq2)

exception Echec_unif of string

(* zipper d'une liste d'équations *)
type equa_zip = equa * equa

(* rembobine le zipper *)
let (* rec *) rembobine (e : equa_zip) =
  match e with
    ([], _) -> e
  | (c::e1, e2) -> (e1, c::e2)

(* remplace unee variable par un type dans un zipper d'équations *)
let substitue_type_zip (e : equa_zip) (v : string) (t0 : ptype) : equa_zip =
  match e with
    (e1, e2) -> (substitue_type_partout e1 v t0, substitue_type_partout e2 v t0)

(* trouve un type associé à une variable dans un zipper d'équation *)
let rec trouve_but (e : equa_zip) (but : string) =
  match e with
    (_, []) -> raise VarPasTrouve
  | (_, (VarType v, t)::_) when v = but -> t
  | (_, (t, VarType v)::_) when v = but -> t
  | (e1, c::e2) -> trouve_but (c::e1, e2) but

let rec print_equa (e : equa) =
  match e with
    [] -> ""
    | (t1, t2)::erest -> (print_type t1) ^ " = " ^ (print_type t2) ^ ", " ^ (print_equa erest)

let print_equa_zip (e : equa_zip) =
  match e with
    (e1, e2) -> "[" ^ (print_equa e1) ^ "] against [" ^ (print_equa e2) ^ "]"

(* résout un système d'équations *)
let rec unification (e : equa_zip) (but : string) : ptype =
  print_endline ("unification of " ^ (print_equa_zip e));
  match e with
    (* on a passé toutes les équations : succes *)
    (_, []) -> (try trouve_but (rembobine e) but with VarPasTrouve -> raise (Echec_unif "but pas trouvé"))
    (* equation avec but : on passe *)
  | (e1, (VarType v1, t2)::e2) when v1 = but ->  unification ((VarType v1, t2)::e1, e2) but
    (* deux variables : remplacer l'une par l'autre *)
  | (e1, (VarType v1, VarType v2)::e2) ->  unification (substitue_type_zip (rembobine (e1,e2)) v2 (VarType v1)) but
    (* une variable à gauche : vérification d'occurence puis remplacement *)
  | (e1, (VarType v1, t2)::e2) ->  if appartient_type v1 t2 then raise (Echec_unif ("occurence de "^ v1 ^" dans "^(print_type t2))) else  unification (substitue_type_zip (rembobine (e1,e2)) v1 t2) but
    (* une variable à droite : vérification d'occurence puis remplacement *)
  | (e1, (t1, VarType v2)::e2) ->  if appartient_type v2 t1 then raise (Echec_unif ("occurence de "^ v2 ^" dans " ^(print_type t1))) else  unification (substitue_type_zip (rembobine (e1,e2)) v2 t1) but
    (* types fleche des deux cotes : on decompose  *)
  | (e1, (ArrowType (t1,t2), ArrowType (t3, t4))::e2) -> unification (e1, (t1, t3)::(t2, t4)::e2) but
    (* types fleche à gauche pas à droite : echec  *)
  | (_e1, (ArrowType (_,_), t3)::_e2) -> raise (Echec_unif ("type fleche non-unifiable avec "^(print_type t3)))
    (* types fleche à droite pas à gauche : echec  *)
  | (_e1, (t3, ArrowType (_,_))::_e2) -> raise (Echec_unif ("type fleche non-unifiable avec "^(print_type t3)))
    (* types nat des deux cotes : on passe *)
  | (e1, (NatType, NatType)::e2) -> unification (e1, e2) but
    (* types nat à gauche pas à droite : échec *)
  | (_e1, (NatType, t3)::_e2) -> raise (Echec_unif ("type entier non-unifiable avec "^(print_type t3)))
    (* types à droite pas à gauche : échec *)
  | (_e1, (t3, NatType)::_e2) -> raise (Echec_unif ("type entier non-unifiable avec "^(print_type t3)))
    (* types liste des deux cotes : on passe *)
  | (e1, (ListType t1, ListType t2)::e2) -> unification (e1, (t1, t2)::e2) but
    (* types liste à gauche pas à droite : échec *)
  | (_e1, (ListType _, t3)::_e2) -> raise (Echec_unif ("type liste non-unifiable avec "^(print_type t3)))
    (* types liste à droite pas à gauche : échec *)
  | (_e1, (t3, ListType _)::_e2) -> raise (Echec_unif ("type liste non-unifiable avec "^(print_type t3)))
    (* types ref des deux cotes : on passe *)
  | (e1, (RefType t1, RefType t2)::e2) -> unification (e1, (t1, t2)::e2) but
    (* types ref à gauche pas à droite : échec *)
  | (_e1, (RefType _, t3)::_e2) -> raise (Echec_unif ("type ref non-unifiable avec "^(print_type t3)))
    (* types ref à droite pas à gauche : échec *)
  | (_e1, (t3, RefType _)::_e2) -> raise (Echec_unif ("type ref non-unifiable avec "^(print_type t3)))
    (* types unit des deux cotes : on passe *)
  | (e1, (Unit, Unit)::e2) -> unification (e1, e2) but
    (* TODO *)
    (* types scheme dans les équations : échec *)
  | (_e1, (SchemeType _, t3)::_e2) -> raise (Echec_unif ("type scheme non-unifiable avec "^(print_type t3)))
  | (_e1, (t3, SchemeType _)::_e2) -> raise (Echec_unif ("type scheme non-unifiable avec "^(print_type t3)))

(* enchaine generation d'equation et unification *)
let inference (t : Common.pterm) : string =
  let e : equa_zip = ([], genere_equa t (VarType "but") []) in
  print_endline ("starting inference with " ^ (Common.print_term t) ^ ", equations: " ^ (print_equa_zip e));
  try (let res = unification e "but" in
       (Common.print_term t)^" ***TYPABLE*** avec le type "^(print_type res))
  with Echec_unif bla -> (Common.print_term t)^" ***PAS TYPABLE*** : "^bla
