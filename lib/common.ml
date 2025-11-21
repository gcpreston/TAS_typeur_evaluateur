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
  (* Branching *)
  | IfZero of pterm * pterm * pterm
  | IfEmpty of pterm * pterm * pterm
  (* let x = e1 in e2 *)
  | Let of string * pterm * pterm
  | Ref of pterm
  | Deref of pterm
  | Assign of pterm * pterm
  (* Recusion *)
  | Fix of string * pterm

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
    | IfZero (c, t, f) -> "(ifzero " ^ (print_term c) ^ " " ^ (print_term t) ^ " " ^ (print_term f) ^ ")"
    | IfEmpty (c, t, f) -> "(ifempty " ^ (print_term c) ^ " " ^ (print_term t) ^ " " ^ (print_term f) ^ ")"
    | Let (x, e1, e2) -> "let " ^ x ^ " = " ^ (print_term e1) ^ " in " ^ (print_term e2)
    | Ref e -> "ref " ^ (print_term e)
    | Deref e -> "!" ^ (print_term e)
    | Assign (e1, e2) -> (print_term e1) ^ " := " ^ (print_term e2)
    | Fix (phi, m) -> "fix (" ^ phi ^ " -> " ^ (print_term m) ^ ")"

and print_list (l : pterm) : string =
  match l with
    | EmptyList -> "[]"
    | Cons (hd, tl) ->  "[" ^ (print_list_inner (Cons (hd, tl))) ^ "]"
    | _ -> print_term l

and print_list_inner (l : pterm) : string =
  match l with
    | Cons (hd, EmptyList) -> print_term hd
    | Cons (hd, tl) -> print_term hd ^ ", " ^ print_list_inner tl
    | _ -> print_term l
