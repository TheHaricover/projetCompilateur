{
  open Lexing
  open Parser
  open Lang
  exception Lexerror

  let pos lexbuf = (lexeme_start lexbuf, lexeme_end lexbuf)

  let advance_line_pos pos =
    { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum; }

  let advance_line lexbuf =
    lexbuf.lex_curr_p <- advance_line_pos lexbuf.lex_curr_p

}

let includeline = '#' [^ '\n']* '\n'
let alph =           ['a'-'z''A'-'Z']
let num  =           ['0'-'9'] 
let decimal	=	'0'|(['1'-'9']['0'-'9']*)
let comment = '/' '*' [^'*']* ('*' (('*'*)|([^'*''/'][^'*']*)))* '*' '/'

rule token = parse
 [' ' '\t' '\n']
    { token lexbuf }    (* white space: recursive call of lexer *)
| includeline
    { advance_line lexbuf; token lexbuf }    (* C include directives --> ignore *)
| comment
    { token lexbuf }    (* comment --> ignore *)
| decimal  as i	  { INTCONSTANT (int_of_string i)}
| '+'  { PLUS }
| '-'  { MINUS }
| '*'  { TIMES }
| '/'  { DIV }
| '%'  { MOD }
| '('  { LPAREN }
| ')'  { RPAREN }
| '{'  { LBRACE }
| '}'  { RBRACE }
| '='  { EQ }
| ','  { COMMA }
| ';'  { SEMICOLON }
| ':'  { COLON }
| '?'  { QMARK }

| "bool"       {TP BoolT}
| "int"        {TP IntT}
| "void"       {TP VoidT}

| "true"       {BCONSTANT true}
| "false"      {BCONSTANT false}
| "if"         {IF}
| "else"       {ELSE}
| "while"      {WHILE}
| "for"        {FOR}
| "return"     {RETURN}

| "=="         {BCEQ}
| ">="         {BCGE}
| '>'          {BCGT}
| "<="         {BCLE}
| '<'          {BCLT}
| "!="         {BCNE}

| "&&"         {BLAND}
| "||"         {BLOR}

| eof          {EOF}

| alph(alph|num)* as i  {IDENTIFIER i}

| _  {Printf.printf "ERROR: unrecogized symbol '%s'\n" (Lexing.lexeme lexbuf);
      raise Lexerror }

and
    ruleTail acc = parse
| eof { acc }
| _* as str { ruleTail (acc ^ str) lexbuf }
