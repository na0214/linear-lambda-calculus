type term =
    Var of string
  | Boolean of Type.qual * bool
  | If of term * term * term
  | Pair of Type.qual * term * term
  | Split of term * string * string * term
  | Abs of Type.qual * string * Type.ltype * term
  | App of term * term;;

type toplevel = (string * term) list;;

let rec print_ast (t : term) =
  match t with
    Var s -> "Var " ^ s
  | Boolean (q,b) -> Type.qual_of_string q ^ " " ^ string_of_bool b
  | If (t1,t2,t3) -> "If (" ^ print_ast t1 ^ ") (" ^ print_ast t2 ^ ") (" ^ print_ast t3 ^ ")"
  | Pair (q,t1,t2) -> Type.qual_of_string q ^ " (" ^ print_ast t1 ^ "," ^ print_ast t2 ^ ")"
  | Split (t1,x,y,t2) -> "Split (" ^ print_ast t1 ^ ") (" ^ x ^ "," ^ y ^ ") (" ^ print_ast t2 ^ ")"
  | Abs (q,n,tp,t1) -> Type.qual_of_string q ^ " Abs (" ^ n ^ ") (" ^ Type.type_of_string tp ^ ") (" ^ print_ast t1 ^ ")"
  | App (t1,t2) -> "App (" ^ print_ast t1 ^ ") (" ^ print_ast t2 ^ ")";;
