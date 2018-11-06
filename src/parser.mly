%token <string> ID
%token <Type.qual> QUAL
%token <bool> BOOL
%token LET EQ SEMICOLON IF THEN ELSE COMMA LEFT RIGHT
%token DOT LAMBDA COLON MULTI ARROW IN SPLIT AS BoolT Eof

%type <Ast.toplevel> toplevel
%type <Ast.term> term simpleterm
%type <Type.ltype> qualtype type

%start toplevel

%%

toplevel:
      LET ID EQ term SEMICOLON toplevel { ($2,$4) :: $6 }
    | { [] }
    | Eof { [] }
    | error
    { failwith
        (Printf.sprintf "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ()))
    }
    ;

term:
      term simpleterm { Ast.App ($1,$2) }
    | simpleterm { $1 }
    ;

simpleterm:
      ID { Ast.Var $1 }
    | QUAL BOOL { Ast.Boolean ($1,(if $2 then Ast.True else Ast.False)) }
    | IF term THEN term ELSE term { Ast.If ($2,$4,$6) }
    | QUAL LEFT term COMMA term RIGHT { Ast.Pair ($1,$3,$5) }
    | SPLIT term AS ID COMMA ID IN term { Ast.Split ($2,$4,$6,$8) }
    | QUAL LAMBDA ID COLON qualtype DOT term { Ast.Abs ($1,$3,$5,$7) }
    ;

qualtype:
     QUAL type
     {
        (match $2 with
           Type.Bool _ -> Type.Bool $1
         | Type.Pair (_,t1,t2) -> Type.Pair ($1,t1,t2)
         | Type.Fn (_,t1,t2) -> Type.Fn ($1,t1,t2))
     }
     ;

type:
      BoolT { Type.Bool Type.Un }
    | qualtype MULTI qualtype { Type.Pair (Type.Un,$1,$3) }
    | qualtype ARROW qualtype { Type.Fn (Type.Un,$1,$3) }
    ;
