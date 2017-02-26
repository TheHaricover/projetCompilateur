type token =
  | IDENTIFIER of (string)
  | TP of (Lang.tp)
  | BCONSTANT of (bool)
  | INTCONSTANT of (int)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | EQ
  | COMMA
  | SEMICOLON
  | COLON
  | QMARK
  | IF
  | ELSE
  | WHILE
  | FOR
  | RETURN
  | BCEQ
  | BCGE
  | BCGT
  | BCLE
  | BCLT
  | BCNE
  | BLAND
  | BLOR
  | EOF

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int Lang.prog
