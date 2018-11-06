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

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  260 (* LET *);
  261 (* EQ *);
  262 (* SEMICOLON *);
  263 (* IF *);
  264 (* THEN *);
  265 (* ELSE *);
  266 (* COMMA *);
  267 (* LEFT *);
  268 (* RIGHT *);
  269 (* DOT *);
  270 (* LAMBDA *);
  271 (* COLON *);
  272 (* MULTI *);
  273 (* ARROW *);
  274 (* IN *);
  275 (* SPLIT *);
  276 (* AS *);
  277 (* BoolT *);
  278 (* Eof *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* QUAL *);
  259 (* BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\005\000\005\000\005\000\
\000\000"

let yylen = "\002\000\
\006\000\000\000\001\000\001\000\002\000\001\000\001\000\002\000\
\006\000\006\000\008\000\007\000\002\000\001\000\003\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\004\000\000\000\003\000\017\000\000\000\000\000\
\007\000\000\000\000\000\000\000\000\000\006\000\008\000\000\000\
\000\000\000\000\000\000\000\000\005\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\010\000\014\000\000\000\013\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\016\000\
\000\000"

let yydgoto = "\002\000\
\006\000\013\000\014\000\033\000\039\000"

let yysindex = "\001\000\
\003\255\000\000\000\000\007\255\000\000\000\000\016\255\062\255\
\000\000\085\255\062\255\062\255\036\255\000\000\000\000\062\255\
\011\255\049\255\028\255\003\255\000\000\012\255\245\254\062\255\
\023\255\000\000\062\255\015\255\051\255\017\255\060\255\255\254\
\021\255\062\255\043\255\000\000\000\000\024\255\000\000\062\255\
\062\255\031\255\015\255\015\255\062\255\062\255\000\000\000\000\
\062\255"

let yyrindex = "\000\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\046\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\065\255\000\000\000\000\000\000\074\255\000\000\000\000\000\000\
\081\255"

let yygindex = "\000\000\
\039\000\255\255\243\255\022\000\000\000"

let yytablesize = 101
let yytable = "\021\000\
\032\000\001\000\003\000\028\000\021\000\021\000\004\000\007\000\
\021\000\018\000\019\000\023\000\009\000\010\000\022\000\021\000\
\032\000\021\000\011\000\037\000\008\000\027\000\029\000\030\000\
\005\000\031\000\035\000\021\000\009\000\010\000\012\000\021\000\
\041\000\040\000\011\000\021\000\009\000\010\000\045\000\043\000\
\044\000\020\000\011\000\042\000\049\000\002\000\012\000\025\000\
\046\000\009\000\010\000\009\000\010\000\038\000\012\000\011\000\
\024\000\011\000\026\000\034\000\009\000\010\000\009\000\010\000\
\047\000\048\000\011\000\012\000\011\000\012\000\009\000\036\000\
\009\000\009\000\009\000\000\000\009\000\000\000\012\000\012\000\
\012\000\012\000\012\000\012\000\009\000\012\000\011\000\015\000\
\011\000\011\000\011\000\000\000\011\000\012\000\000\000\016\000\
\000\000\000\000\017\000\000\000\011\000"

let yycheck = "\013\000\
\002\001\001\000\000\001\015\001\018\000\019\000\004\001\001\001\
\022\000\011\000\012\000\001\001\001\001\002\001\016\000\029\000\
\002\001\031\000\007\001\021\001\005\001\010\001\024\000\001\001\
\022\001\027\000\010\001\041\000\001\001\002\001\019\001\045\000\
\034\000\013\001\007\001\049\000\001\001\002\001\040\000\016\001\
\017\001\006\001\007\001\001\001\046\000\000\000\019\001\020\001\
\018\001\001\001\002\001\001\001\002\001\032\000\019\001\007\001\
\008\001\007\001\020\000\009\001\001\001\002\001\001\001\002\001\
\043\000\044\000\007\001\019\001\007\001\019\001\006\001\012\001\
\008\001\009\001\010\001\255\255\012\001\255\255\019\001\006\001\
\019\001\008\001\009\001\010\001\020\001\012\001\006\001\003\001\
\008\001\009\001\010\001\255\255\012\001\020\001\255\255\011\001\
\255\255\255\255\014\001\255\255\020\001"

let yynames_const = "\
  LET\000\
  EQ\000\
  SEMICOLON\000\
  IF\000\
  THEN\000\
  ELSE\000\
  COMMA\000\
  LEFT\000\
  RIGHT\000\
  DOT\000\
  LAMBDA\000\
  COLON\000\
  MULTI\000\
  ARROW\000\
  IN\000\
  SPLIT\000\
  AS\000\
  BoolT\000\
  Eof\000\
  "

let yynames_block = "\
  ID\000\
  QUAL\000\
  BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.toplevel) in
    Obj.repr(
# 16 "parser.mly"
                                        ( (_2,_4) :: _6 )
# 167 "parser.ml"
               : Ast.toplevel))
; (fun __caml_parser_env ->
    Obj.repr(
# 17 "parser.mly"
      ( [] )
# 173 "parser.ml"
               : Ast.toplevel))
; (fun __caml_parser_env ->
    Obj.repr(
# 18 "parser.mly"
          ( [] )
# 179 "parser.ml"
               : Ast.toplevel))
; (fun __caml_parser_env ->
    Obj.repr(
# 20 "parser.mly"
    ( failwith
        (Printf.sprintf "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ()))
    )
# 189 "parser.ml"
               : Ast.toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.term) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.term) in
    Obj.repr(
# 28 "parser.mly"
                      ( Ast.App (_1,_2) )
# 197 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.term) in
    Obj.repr(
# 29 "parser.mly"
                 ( _1 )
# 204 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 33 "parser.mly"
         ( Ast.Var _1 )
# 211 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Type.qual) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 34 "parser.mly"
                ( Ast.Boolean (_1,(if _2 then Ast.True else Ast.False)) )
# 219 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Ast.term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.term) in
    Obj.repr(
# 35 "parser.mly"
                                  ( Ast.If (_2,_4,_6) )
# 228 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Type.qual) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.term) in
    Obj.repr(
# 36 "parser.mly"
                                      ( Ast.Pair (_1,_3,_5) )
# 237 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : Ast.term) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.term) in
    Obj.repr(
# 37 "parser.mly"
                                        ( Ast.Split (_2,_4,_6,_8) )
# 247 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Type.qual) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Type.ltype) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.term) in
    Obj.repr(
# 38 "parser.mly"
                                             ( Ast.Abs (_1,_3,_5,_7) )
# 257 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Type.qual) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Type.ltype) in
    Obj.repr(
# 43 "parser.mly"
     (
        (match _2 with
           Type.Bool _ -> Type.Bool _1
         | Type.Pair (_,t1,t2) -> Type.Pair (_1,t1,t2)
         | Type.Fn (_,t1,t2) -> Type.Fn (_1,t1,t2))
     )
# 270 "parser.ml"
               : Type.ltype))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
            ( Type.Bool Type.Un )
# 276 "parser.ml"
               : Type.ltype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Type.ltype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Type.ltype) in
    Obj.repr(
# 53 "parser.mly"
                              ( Type.Pair (Type.Un,_1,_3) )
# 284 "parser.ml"
               : Type.ltype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Type.ltype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Type.ltype) in
    Obj.repr(
# 54 "parser.mly"
                              ( Type.Fn (Type.Un,_1,_3) )
# 292 "parser.ml"
               : Type.ltype))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.toplevel)
