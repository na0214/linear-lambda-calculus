type term =
    Var of string
  | Boolean of Type.qual * bool
  | If of term * term * term
  | Pair of Type.qual * term * term
  | Split of term * string * string * term
  | Abs of Type.qual * string * Type.ltype * term
  | App of term * term;;

type toplevel = (string * term) list;;

val print_ast : term -> string;;
