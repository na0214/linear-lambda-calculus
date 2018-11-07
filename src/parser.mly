%token <string> ID
%token <Type.qual> QUAL
%token <bool> BOOL
%token LET EQ SEMICOLON IF THEN ELSE COMMA LEFT RIGHT
%token DOT LAMBDA COLON MULTI ARROW IN SPLIT AS BoolT Eof LPAREN RPAREN

%right ARROW
%right QUAL

%type <Ast.toplevel> toplevel
%type <Ast.term> term simpleterm
%type <Type.ltype> qualtype

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
    | QUAL BOOL { Ast.Boolean ($1,$2) }
    | IF term THEN term ELSE term { Ast.If ($2,$4,$6) }
    | QUAL LEFT term COMMA term RIGHT { Ast.Pair ($1,$3,$5) }
    | SPLIT term AS ID COMMA ID IN term { Ast.Split ($2,$4,$6,$8) }
    | QUAL LAMBDA ID COLON qualtype DOT term { Ast.Abs ($1,$3,$5,$7) }
    | LPAREN term RPAREN { $2 }
    ;



qualtype:
      QUAL BoolT { Type.Bool $1 }
    | QUAL LPAREN qualtype MULTI qualtype RPAREN { Type.Pair ($1,$3,$5) }
    | QUAL LPAREN qualtype ARROW qualtype RPAREN { Type.Fn ($1,$3,$5) }
    | LPAREN qualtype RPAREN { $2 }
    ;
