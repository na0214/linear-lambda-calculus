type token =
  | ID of (string)
  | QUAL of (Type.qual)
  | BOOL of (bool)
  | LET
  | EQ
  | SEMICOLON
  | IF
  | THEN
  | ELSE
  | COMMA
  | LEFT
  | RIGHT
  | DOT
  | LAMBDA
  | COLON
  | MULTI
  | ARROW
  | IN
  | SPLIT
  | AS
  | BoolT
  | Eof
  | LPAREN
  | RPAREN

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.toplevel
