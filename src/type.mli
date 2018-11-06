type qual = Lin | Un;;

type ltype =
    Bool of qual
  | Pair of qual * ltype * ltype
  | Fn of qual * ltype * ltype;;

type context = (string * ltype) list;;

val get_qual : ltype -> qual;;
val check_qual_contain_type : qual -> ltype -> bool;;
val check_qual_contain_context : qual -> context -> bool;;
val qual_of_string : qual -> string;;
val type_of_string : ltype -> string;;
