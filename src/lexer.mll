{
    open Parser;;
}

rule token = parse
            [' ' '\t' '\n'] { token lexbuf }
          | "true" { BOOL(true) }
          | "false" { BOOL(false) }
          | "lin" { QUAL(Type.Lin) }
          | "un" { QUAL(Type.Un) }
          | "let" { LET }
          | '=' { EQ }
          | ';' { SEMICOLON }
          | "if" { IF }
          | "then" { THEN }
          | "else" { ELSE }
          | ',' { COMMA }
          | '<' { LEFT }
          | '>' { RIGHT }
          | '.' { DOT }
          | '#' { LAMBDA }
          | ':' { COLON }
          | '*' { MULTI }
          | '(' { LPAREN }
          | ')' { RPAREN }
          | "->" { ARROW }
          | "in" { IN }
          | "split" { SPLIT }
          | "as" { AS }
          | "bool" { BoolT }
          | ['a'-'z' 'A'-'Z' '0'-'9']+ { ID(Lexing.lexeme lexbuf) }
          | eof { Eof }
