let input_file
  = if Array.length Sys.argv = 1 then exit 0 else open_in Sys.argv.(1)

let rec exec_ast toplv con =
  match toplv with
    [] -> con
  | (n,t) :: xs -> let (t',c) = Typecheck.type_check t con in print_string (Ast.print_ast t ^ " : "); Type.print_context (((n,t') :: c));
    let l = exec_ast xs ((n,t') :: c) in
    if List.mem_assoc n c then l @ con else con @ (List.remove_assoc n l);;


let () =
  let top = Parser.toplevel Lexer.token (Lexing.from_channel input_file) in
  let con = exec_ast top [] in
  let unused = List.filter (fun (s,x) -> Type.get_qual x = Type.Lin && s <> "main") con in
  if List.length unused = 0 then Type.print_context con else raise (Typecheck.UnUsedError (fst (List.hd unused)));;
