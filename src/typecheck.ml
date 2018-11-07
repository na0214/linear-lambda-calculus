type checkT = Type.ltype * Type.context;;

exception TypeError of string * string;;
exception ContextError;;
exception QualError;;
exception UnUsedError of string;;

let check_equal (t1 : Type.ltype) (t2 : Type.ltype) : unit =
  if t1 = t2 then () else raise (TypeError (Type.type_of_string t1,Type.type_of_string t2));;

let check_equal' (t1 : Type.ltype) (t2 : Type.ltype) : unit =
  let replace_un_qual q_t =
    match q_t with
      Type.Bool q -> Type.Bool Type.Un
    | Type.Pair (q,t1',t2') -> Type.Pair (Type.Un,t1',t2')
    | Type.Fn (q,t1',t2') -> Type.Fn (Type.Un,t1',t2') in
  check_equal (replace_un_qual t1) (replace_un_qual t2);;

let check_equal_const (t1 : Type.ltype) (t2 : Type.ltype) : unit =
  let erase_type t =
    match t with
      Type.Bool _ -> Type.Bool Type.Un
    | Type.Pair (_,_,_) -> Type.Pair (Type.Un,Type.Bool Type.Un,Type.Bool Type.Un)
    | Type.Fn (_,_,_) -> Type.Fn (Type.Un,Type.Bool Type.Un,Type.Bool Type.Un) in
  check_equal (erase_type t1) (erase_type t2);;

let check_equal_context (context1 : Type.context) (context2 : Type.context) : unit =
  if List.length context1 <> List.length context2 then raise ContextError else
    let exists_context c1 c2 = List.fold_left (fun x -> fun (n,_) -> List.exists (fun (y,_) -> n == y) c1 && x) true c2 in
    if exists_context context1 context2 && exists_context context2 context1 then () else raise ContextError;;

let t_var (name : string) (context : Type.context) : checkT =
  let var_type = List.assoc name context in
  let qual = Type.get_qual var_type in
  match qual with
    Type.Un -> (var_type,context)
  | Type.Lin -> (var_type,List.remove_assoc name context);;

let rec type_check (term : Ast.term) (context : Type.context) : checkT =
  match term with
    Ast.Var name -> t_var name context
  | Ast.Boolean (qual,value) -> (Type.Bool qual,context)
  | Ast.If (term1,term2,term3) -> t_if term1 term2 term3 context
  | Ast.Pair (qual,term1,term2) -> t_pair qual term1 term2 context
  | Ast.Split (term1,x,y,term_body) -> t_split term1 x y term_body context
  | Ast.App (term1,term2) -> t_app term1 term2 context
  | Ast.Abs (qual,name,vtype,term_body) -> t_abs qual name vtype term_body context
and t_if (term_cond : Ast.term) (term1 : Ast.term) (term2 : Ast.term) (context : Type.context) : checkT =
  let (cond_t,context2) = type_check term_cond context in
  let (term1_t,context3) = type_check term1 context2 in
  let (term2_t,context3') = type_check term2 context2 in
  check_equal' cond_t (Type.Bool Type.Un);
  check_equal term1_t term2_t;
  check_equal_context context3 context3;
  (term1_t,context3)
and t_pair (qual : Type.qual) (term1 : Ast.term) (term2 : Ast.term) (context : Type.context) : checkT =
  let (term1_t,context2) = type_check term1 context in
  let (term2_t,context3) = type_check term2 context2 in
  if not ((Type.check_qual_contain_type qual term1_t) && (Type.check_qual_contain_type qual term2_t)) then raise QualError else (Type.Pair (qual,term1_t,term2_t),context3)
and t_split (term1 : Ast.term) (x : string) (y : string) (term_body : Ast.term) (context : Type.context) : checkT =
  let (term1_t,context2) = type_check term1 context in check_equal_const term1_t (Type.Pair (Type.Un,Type.Bool Type.Un,Type.Bool Type.Un));
  let Type.Pair (_,x_t,y_t) = term1_t in
  let (term_body_t,context3) = type_check term_body ((x,x_t) :: (y,y_t) :: context2) in (term_body_t,List.remove_assoc y (List.remove_assoc x context3))
and t_app (term1 : Ast.term) (term2 : Ast.term) (context : Type.context) : checkT =
  let (term1_t,context2) = type_check term1 context in check_equal_const term1_t (Type.Fn (Type.Un,Type.Bool Type.Un,Type.Bool Type.Un));
  let (term2_t,context3) = type_check term2 context2 in
  let (Type.Fn (_,t11,t12)) = term1_t in check_equal t11 term2_t; (t12,context3)
and t_abs (qual : Type.qual) (name : string) (vtype : Type.ltype) (term_body : Ast.term) (context : Type.context) : checkT =
  if Type.check_qual_contain_type (Type.get_qual vtype) vtype then () else raise QualError;
  let (term_body_t,context2) = type_check term_body ((name,vtype) :: context) in
  if Type.check_qual_contain_type qual term_body_t then () else raise QualError;
  if Type.get_qual vtype = Type.Lin && List.mem_assoc name context2 then raise (UnUsedError name) else ();
  if qual = Type.Un then check_equal_context context (List.remove_assoc name context2) else ();
  (Type.Fn (qual,vtype,term_body_t),List.remove_assoc name context2);;
