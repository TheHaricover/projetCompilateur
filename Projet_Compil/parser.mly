%{
open Lang
%}

%token <string> IDENTIFIER
%token <Lang.tp> TP
%token <bool> BCONSTANT
%token <int> INTCONSTANT
%token PLUS MINUS TIMES DIV MOD
%token LPAREN RPAREN LBRACE RBRACE
%token EQ COMMA SEMICOLON COLON QMARK
%token IF ELSE WHILE FOR RETURN BCEQ BCGE BCGT BCLE BCLT BCNE BLAND BLOR
%token EOF

%right ELSE

%start start
%type <int Lang.prog> start

%%

start: prog { $1 }
;

prog:
  var_or_fun_list EOF
  {Prog (fst $1, snd $1) }
;


var_or_fun_list: 
  var_or_fun
  { $1 }
| var_or_fun_list var_or_fun
    { (fst $1 @ fst $2, snd $1 @ snd $2) }
;

var_or_fun:
  vardecl SEMICOLON
  { ([$1], []) }
| fundefn
    { ([], [$1]) }
;

fundefn: 
  fundecl LBRACE vardecl_semi_list_opt block_item_list_opt RBRACE
  { Fundefn($1, $3, $4) }
;

fundecl: TP IDENTIFIER LPAREN vardecl_comma_list_opt RPAREN 
  { Fundecl($1, $2, $4) }
;

vardecl_comma_list_opt:
  /* empty */
  { [] }
| vardecl_comma_list
    { $1 }
;

/* comma-separated list of vardecls */
vardecl_comma_list:
    vardecl
    { [$1] }
| vardecl_comma_list COMMA vardecl
    { $1 @ [$3] }
;

vardecl_semi_list_opt:
    /* empty */
  { [] }
| vardecl_semi_list SEMICOLON
    { $1 }
;

/* semicolon-separated list of vardecls */
vardecl_semi_list:
    vardecl
    { [$1] }
| vardecl_semi_list SEMICOLON vardecl
    { $1 @ [$3] }
;

vardecl: TP IDENTIFIER
    { Vardecl($1, $2) }
;



/* *******  EXPRESSIONS  ******* */

primary_expression: 
  IDENTIFIER
    { VarE(0, Var(Local, $1)) }
| BCONSTANT
    { Const(0, BoolV $1) }
| INTCONSTANT
    { Const(0, IntV $1) }
    /* OMITTED: string-literal */
| LPAREN expression RPAREN
    { $2 }
| LPAREN expression QMARK expression COLON expression RPAREN
    { IfThenElse (0, $2, $4, $6) }
    ;
  
/* SIMPLIFIED: Function calls only with function names */
postfix_expression:
      primary_expression
      { $1 }
  | IDENTIFIER LPAREN argument_expression_list_opt RPAREN
      { CallE(0, $1, $3) }
      ;

argument_expression_list_opt:
      /* empty */
    { [] }
  | argument_expression_list
      { $1 }
;

argument_expression_list:
    expression 
      { [$1] }
  | argument_expression_list COMMA expression
      { $1 @ [$3] }
;

unary_expression: 
 postfix_expression
      { $1 }
;

/* TODO: cast currently ignored */
cast_expression:
 unary_expression
      { $1 }
| LPAREN TP RPAREN cast_expression
      { $4 }
;

multiplicative_expression:
      cast_expression
      { $1 }
| multiplicative_expression TIMES cast_expression
    { BinOp(0, BArith BAmul, $1, $3) }
| multiplicative_expression DIV cast_expression
    { BinOp(0, BArith BAdiv, $1, $3) }
| multiplicative_expression MOD cast_expression 
    { BinOp(0, BArith BAmod, $1, $3) }
;

additive_expression:
    multiplicative_expression
    { $1 }
| additive_expression PLUS multiplicative_expression
    { BinOp(0, BArith BAadd, $1, $3) }
| additive_expression MINUS multiplicative_expression
    { BinOp(0, BArith BAsub, $1, $3) }
;

shift_expression: 
    additive_expression
    { $1 }
;

relational_expression:
    shift_expression
    { $1 }
| relational_expression BCLT shift_expression
    { BinOp(0, BCompar BClt, $1, $3) }
| relational_expression BCGT shift_expression
    { BinOp(0, BCompar BCgt, $1, $3) }
| relational_expression BCLE shift_expression
    { BinOp(0, BCompar BCle, $1, $3) }
| relational_expression BCGE shift_expression
    { BinOp(0, BCompar BCge, $1, $3) }
;

equality_expression:
    relational_expression
    { $1 }
| equality_expression BCEQ relational_expression
    { BinOp(0, BCompar BCeq, $1, $3) }
| equality_expression BCNE relational_expression
    { BinOp(0, BCompar BCne, $1, $3) }
;

logical_AND_expression:
    equality_expression
    { $1 }
| logical_AND_expression BLAND equality_expression
    { IfThenElse(0, $1, $3, Const(0, BoolV false)) }
/*  It is better to use IfThenElse instead of boolean and (the same holds for or)
    because the bytecode and is not evaluated sequentially 
    --> possibly non-termination of programs
  { BinOp(0, BLogic BLand, $1, $3) } 
*/
;

logical_OR_expression: 
    logical_AND_expression 
    { $1 }
| logical_OR_expression BLOR logical_AND_expression
    { IfThenElse(0, $1, Const(0, BoolV true), $3) }
/*    { BinOp(0, BLogic BLor, $1, $3) } */
;


/* OMITTED AND-expr .. inclusive-or-expr, conditional-expr */

expression:
    logical_OR_expression
    { $1 }
    /* OMITTED: expression , assignment_expression */
;


/* *******  STATEMENTS  ******* */

statement: 
    compound_statement { $1 }
|   expression_statement { $1 }
|   selection_statement  { $1 }
|   iteration_statement { $1 }
|   jump_statement { $1 }
;
/* OMITTED: labeled-statement */


compound_statement: 
    LBRACE block_item_list_opt RBRACE
    { $2 }
;

block_item_list_opt:
    /* empty */
  { Skip}
| block_item_list
    { $1 }
;

block_item_list: 
    block_item
    { $1 }
| block_item_list block_item
    { Seq ($1, $2) }
;

/* CHANGED: assignment instead of expression */

expression_statement: 
    assignment SEMICOLON { $1 }
|   call_statement SEMICOLON { $1 }
;

/* NEW */
call_statement:
    IDENTIFIER LPAREN argument_expression_list_opt RPAREN
    { CallC($1, $3) }
;

/* CHANGED: only assignment to variables; 
   RENAMED assignment-expression TO assignment
   assignment a statement and not an expression */
assignment:
  IDENTIFIER EQ expression
  { Assign(0, Var(Local, $1), $3) }
;

/*
assignment_operator: EQ
 OMITTED the remaining *= /= %= += -= <<= >>= &= ?= |= 
*/


/* OMITTED: declaration */
block_item:  statement { $1 }
;

/* grammar disambiguated by %prec declaration */
selection_statement: 
  IF LPAREN expression RPAREN statement %prec ELSE
  { Cond($3, $5, Skip) }
| IF LPAREN expression RPAREN statement ELSE statement 
    { Cond($3, $5, $7) }
;
       /* OMITTED switch */

iteration_statement: 
  WHILE LPAREN expression RPAREN statement 
  { While($3, $5) }
| FOR LPAREN assignment 
      SEMICOLON expression 
      SEMICOLON assignment RPAREN 
  statement
    { Seq ($3, While ($5, Seq ($9, $7))) }
;
/* simplified FOR */

jump_statement: 
  RETURN SEMICOLON
  { Return (Const (0, VoidV)) }
| RETURN expression SEMICOLON
    { Return $2 }
;

/* OMITTED all but return*/

