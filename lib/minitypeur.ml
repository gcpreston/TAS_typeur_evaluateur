(* Termes *)
type pterm = Var of string
  | App of pterm * pterm
  | Abs of string * pterm
  (* Nats and Nat operations *)
  | N of int
  | Add of pterm * pterm
  | Sub of pterm * pterm
  (* Lists and List operations *)
  | EmptyList
  | Cons of pterm * pterm
  | Head of pterm
  | Tail of pterm

(* Types *)
type ptype = VarType of string
  | ArrowType of ptype * ptype
  | NatType
  | ListType of ptype

(* Environnements de typage *)
type env = (string * ptype) list

(* Listes d'équations *)
type equa = (ptype * ptype) list

exception Echec_print of string

(* pretty printer de termes*)
let rec print_term (t : pterm) : string =
  match t with
    Var x -> x
    | App (t1, t2) -> "(" ^ (print_term t1) ^" "^ (print_term t2) ^ ")"
    | Abs (x, t) -> "(fun "^ x ^" -> " ^ (print_term t) ^")"
    | N n -> string_of_int n
    | Add (t1, t2) -> "(" ^ (print_term t1) ^" + "^ (print_term t2) ^ ")"
    | Sub (t1, t2) -> "(" ^ (print_term t1) ^" - "^ (print_term t2) ^ ")"
    | EmptyList -> print_list EmptyList
    | Cons (hd, tl) -> print_list (Cons (hd, tl))
    | Head l -> "(hd " ^ (print_list l) ^ ")"
    | Tail l -> "(tl " ^ (print_list l) ^ ")"

and print_list (l : pterm) : string =
  match l with
    | EmptyList -> "[]"
    | Cons (hd, tl) ->  "[" ^ (print_list_inner (Cons (hd, tl))) ^ "]"
    | _ -> raise (Echec_print "print_list expected a Cons or EmptyList")

and print_list_inner (l : pterm) : string =
  match l with
    | Cons (hd, EmptyList) -> print_term hd
    | Cons (hd, tl) -> print_term hd ^ ", " ^ print_list_inner tl
    | _ -> raise (Echec_print "print_list_inner expected a Cons")

(* pretty printer de types*)
let rec print_type (t : ptype) : string =
  match t with
    VarType x -> x
  | ArrowType (t1, t2) -> "(" ^ (print_type t1) ^" -> "^ (print_type t2) ^")"
  | NatType -> "NatType"
  | ListType lt -> "[" ^ (print_type lt) ^ "]"

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
  | _ -> false

(* remplace une variable par un type dans type *)
let rec substitue_type (t : ptype) (v : string) (t0 : ptype) : ptype =
  match t with
    VarType v1 when v1 = v -> t0
  | VarType v2 -> VarType v2
  | ArrowType (t1, t2) -> ArrowType (substitue_type t1 v t0, substitue_type t2 v t0)
  | NatType -> NatType
  | ListType lt -> ListType (substitue_type lt v t0)

(* remplace une variable par un type dans une liste d'équations*)
let substitue_type_partout (e : equa) (v : string) (t0 : ptype) : equa =
  List.map (fun (x, y) -> (substitue_type x v t0, substitue_type y v t0)) e

(* genere des equations de typage à partir d'un terme *)
let rec genere_equa (te : pterm) (ty : ptype) (e : env) : equa =
  match te with
    Var v -> let tv : ptype = cherche_type v e in [(ty, tv)]
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

(* enchaine generation d'equation et unification *)
let inference (t : pterm) : string =
  let e : equa_zip = ([], genere_equa t (VarType "but") []) in
  print_endline ("starting inference with " ^ (print_equa_zip e));
  try (let res = unification e "but" in
       (print_term t)^" ***TYPABLE*** avec le type "^(print_type res))
  with Echec_unif bla -> (print_term t)^" ***PAS TYPABLE*** : "^bla
