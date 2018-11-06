type qual = Lin | Un;;

type ltype =
    Bool of qual
  | Pair of qual * ltype * ltype
  | Fn of qual * ltype * ltype;;

type context = (string * ltype) list;;

let get_qual (t : ltype) : qual =
  match t with
    Bool q -> q
  | Pair (q,_,_) -> q
  | Fn (q,_,_) -> q;;

let qual_of_string (q : qual) : string =
  match q with
    Lin -> "lin"
  | Un -> "un";;

let rec type_of_string (t : ltype) : string =
  match t with
    Bool q -> qual_of_string q ^ " Bool"
  | Pair (q,t1,t2) -> qual_of_string q ^ " (" ^ type_of_string t1 ^ "," ^ type_of_string t2 ^ ")"
  | Fn (q,t1,t2) -> qual_of_string q ^ " (" ^ type_of_string t1 ^ "->" ^ type_of_string t2 ^ ")";;

let check_qual_contain_type' (q : qual) (q' : qual) : bool = not (q = Un && q' = Lin);;

let rec check_qual_contain_type (q : qual) (t : ltype) : bool =
  match t with
    Bool q' -> check_qual_contain_type q' t
  | Pair (q',t1,t2) -> (check_qual_contain_type q t1) && (check_qual_contain_type q t2) && (check_qual_contain_type' q q')
  | Fn (q',t1,t2) -> (check_qual_contain_type q t1) && (check_qual_contain_type q t2) && (check_qual_contain_type' q q');;

let check_qual_contain_context (q : qual) (con : context) : bool =
  List.for_all (function (_,t) -> check_qual_contain_type q t) con;;
