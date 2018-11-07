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
  279 (* LPAREN *);
  280 (* RPAREN *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* QUAL *);
  259 (* BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\004\000\004\000\004\000\
\004\000\000\000"

let yylen = "\002\000\
\006\000\000\000\001\000\001\000\002\000\001\000\001\000\002\000\
\006\000\006\000\008\000\007\000\003\000\002\000\006\000\006\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\004\000\000\000\003\000\018\000\000\000\000\000\
\007\000\000\000\000\000\000\000\000\000\000\000\006\000\008\000\
\000\000\000\000\000\000\000\000\000\000\000\000\005\000\000\000\
\000\000\000\000\000\000\013\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\017\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\
\016\000"

let yydgoto = "\002\000\
\006\000\014\000\015\000\037\000"

let yysindex = "\001\000\
\057\255\000\000\000\000\026\255\000\000\000\000\003\255\076\255\
\000\000\027\255\076\255\076\255\076\255\010\255\000\000\000\000\
\076\255\033\255\035\255\043\255\002\255\057\255\000\000\046\255\
\040\255\076\255\058\255\000\000\000\000\076\255\255\254\066\255\
\050\255\069\255\028\255\255\254\051\255\076\255\071\255\000\000\
\000\000\255\254\056\255\076\255\076\255\064\255\007\255\000\000\
\076\255\076\255\255\254\255\254\076\255\060\255\062\255\000\000\
\000\000"

let yyrindex = "\000\000\
\074\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\074\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\088\255\000\000\000\000\000\000\
\095\255\000\000\000\000\000\000\108\255\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\065\000\002\000\242\255\085\000"

let yytablesize = 137
let yytable = "\023\000\
\035\000\001\000\009\000\010\000\023\000\023\000\023\000\008\000\
\011\000\023\000\009\000\010\000\019\000\020\000\021\000\022\000\
\011\000\023\000\024\000\023\000\012\000\036\000\051\000\052\000\
\013\000\028\000\007\000\032\000\012\000\016\000\023\000\034\000\
\013\000\025\000\023\000\009\000\010\000\017\000\023\000\045\000\
\018\000\011\000\026\000\009\000\010\000\049\000\009\000\010\000\
\041\000\011\000\042\000\053\000\011\000\012\000\031\000\030\000\
\003\000\013\000\033\000\039\000\004\000\012\000\027\000\044\000\
\012\000\013\000\009\000\010\000\013\000\009\000\010\000\046\000\
\011\000\002\000\038\000\011\000\009\000\010\000\005\000\048\000\
\040\000\050\000\011\000\056\000\012\000\057\000\029\000\012\000\
\013\000\000\000\000\000\013\000\000\000\009\000\012\000\009\000\
\009\000\009\000\013\000\009\000\012\000\000\000\012\000\012\000\
\012\000\000\000\012\000\009\000\000\000\000\000\000\000\009\000\
\000\000\011\000\012\000\011\000\011\000\011\000\012\000\011\000\
\043\000\000\000\000\000\000\000\000\000\000\000\047\000\011\000\
\000\000\000\000\000\000\011\000\000\000\000\000\000\000\054\000\
\055\000"

let yycheck = "\014\000\
\002\001\001\000\001\001\002\001\019\000\020\000\021\000\005\001\
\007\001\024\000\001\001\002\001\011\000\012\000\013\000\006\001\
\007\001\032\000\017\000\034\000\019\001\023\001\016\001\017\001\
\023\001\024\001\001\001\026\000\019\001\003\001\045\000\030\000\
\023\001\001\001\049\000\001\001\002\001\011\001\053\000\038\000\
\014\001\007\001\008\001\001\001\002\001\044\000\001\001\002\001\
\021\001\007\001\023\001\050\000\007\001\019\001\015\001\010\001\
\000\001\023\001\001\001\010\001\004\001\019\001\020\001\013\001\
\019\001\023\001\001\001\002\001\023\001\001\001\002\001\001\001\
\007\001\000\000\009\001\007\001\001\001\002\001\022\001\024\001\
\012\001\018\001\007\001\024\001\019\001\024\001\022\000\019\001\
\023\001\255\255\255\255\023\001\255\255\006\001\019\001\008\001\
\009\001\010\001\023\001\012\001\006\001\255\255\008\001\009\001\
\010\001\255\255\012\001\020\001\255\255\255\255\255\255\024\001\
\255\255\006\001\020\001\008\001\009\001\010\001\024\001\012\001\
\036\000\255\255\255\255\255\255\255\255\255\255\042\000\020\001\
\255\255\255\255\255\255\024\001\255\255\255\255\255\255\051\000\
\052\000"

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
  LPAREN\000\
  RPAREN\000\
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
# 19 "parser.mly"
                                        ( (_2,_4) :: _6 )
# 186 "parser.ml"
               : Ast.toplevel))
; (fun __caml_parser_env ->
    Obj.repr(
# 20 "parser.mly"
      ( [] )
# 192 "parser.ml"
               : Ast.toplevel))
; (fun __caml_parser_env ->
    Obj.repr(
# 21 "parser.mly"
          ( [] )
# 198 "parser.ml"
               : Ast.toplevel))
; (fun __caml_parser_env ->
    Obj.repr(
# 23 "parser.mly"
    ( failwith
        (Printf.sprintf "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ()))
    )
# 208 "parser.ml"
               : Ast.toplevel))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.term) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.term) in
    Obj.repr(
# 31 "parser.mly"
                      ( Ast.App (_1,_2) )
# 216 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.term) in
    Obj.repr(
# 32 "parser.mly"
                 ( _1 )
# 223 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 36 "parser.mly"
         ( Ast.Var _1 )
# 230 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Type.qual) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 37 "parser.mly"
                ( Ast.Boolean (_1,_2) )
# 238 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Ast.term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.term) in
    Obj.repr(
# 38 "parser.mly"
                                  ( Ast.If (_2,_4,_6) )
# 247 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Type.qual) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.term) in
    Obj.repr(
# 39 "parser.mly"
                                      ( Ast.Pair (_1,_3,_5) )
# 256 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : Ast.term) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.term) in
    Obj.repr(
# 40 "parser.mly"
                                        ( Ast.Split (_2,_4,_6,_8) )
# 266 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : Type.qual) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Type.ltype) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.term) in
    Obj.repr(
# 41 "parser.mly"
                                             ( Ast.Abs (_1,_3,_5,_7) )
# 276 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.term) in
    Obj.repr(
# 42 "parser.mly"
                         ( _2 )
# 283 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Type.qual) in
    Obj.repr(
# 48 "parser.mly"
                 ( Type.Bool _1 )
# 290 "parser.ml"
               : Type.ltype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Type.qual) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Type.ltype) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Type.ltype) in
    Obj.repr(
# 49 "parser.mly"
                                                 ( Type.Pair (_1,_3,_5) )
# 299 "parser.ml"
               : Type.ltype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Type.qual) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Type.ltype) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Type.ltype) in
    Obj.repr(
# 50 "parser.mly"
                                                 ( Type.Fn (_1,_3,_5) )
# 308 "parser.ml"
               : Type.ltype))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Type.ltype) in
    Obj.repr(
# 51 "parser.mly"
                             ( _2 )
# 315 "parser.ml"
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
