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

let check_qual_contain_type (q : qual) (t : ltype) : bool = not (q = Un && (get_qual t) = Lin);;

let check_qual_contain_context (q : qual) (con : context) : bool = List.for_all (function (_,t) -> check_qual_contain_type q t) con;;
