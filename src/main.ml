let () =
  let file = open_in ("test/test.lam") in
  let top = Parser.toplevel Lexer.token (Lexing.from_channel file) in
  let s = snd (List.hd top) in
  let t = Typecheck.type_check s [] in ();;
