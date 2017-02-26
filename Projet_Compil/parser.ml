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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Lang
# 40 "parser.ml"
let yytransl_const = [|
  261 (* PLUS *);
  262 (* MINUS *);
  263 (* TIMES *);
  264 (* DIV *);
  265 (* MOD *);
  266 (* LPAREN *);
  267 (* RPAREN *);
  268 (* LBRACE *);
  269 (* RBRACE *);
  270 (* EQ *);
  271 (* COMMA *);
  272 (* SEMICOLON *);
  273 (* COLON *);
  274 (* QMARK *);
  275 (* IF *);
  276 (* ELSE *);
  277 (* WHILE *);
  278 (* FOR *);
  279 (* RETURN *);
  280 (* BCEQ *);
  281 (* BCGE *);
  282 (* BCGT *);
  283 (* BCLE *);
  284 (* BCLT *);
  285 (* BCNE *);
  286 (* BLAND *);
  287 (* BLOR *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENTIFIER *);
  258 (* TP *);
  259 (* BCONSTANT *);
  260 (* INTCONSTANT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\004\000\004\000\006\000\007\000\
\010\000\010\000\011\000\011\000\008\000\008\000\012\000\012\000\
\005\000\013\000\013\000\013\000\013\000\013\000\015\000\015\000\
\016\000\016\000\017\000\017\000\018\000\019\000\019\000\020\000\
\020\000\020\000\020\000\021\000\021\000\021\000\022\000\023\000\
\023\000\023\000\023\000\023\000\024\000\024\000\024\000\025\000\
\025\000\026\000\026\000\014\000\027\000\027\000\027\000\027\000\
\027\000\028\000\009\000\009\000\033\000\033\000\029\000\029\000\
\036\000\035\000\034\000\030\000\030\000\031\000\031\000\032\000\
\032\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\002\000\002\000\001\000\005\000\005\000\
\000\000\001\000\001\000\003\000\000\000\002\000\001\000\003\000\
\002\000\001\000\001\000\001\000\003\000\007\000\001\000\004\000\
\000\000\001\000\001\000\003\000\001\000\001\000\004\000\001\000\
\003\000\003\000\003\000\001\000\003\000\003\000\001\000\001\000\
\003\000\003\000\003\000\003\000\001\000\003\000\003\000\001\000\
\003\000\001\000\003\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\000\000\001\000\001\000\002\000\002\000\002\000\
\004\000\003\000\001\000\005\000\007\000\005\000\009\000\002\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\074\000\001\000\000\000\003\000\000\000\
\006\000\000\000\000\000\002\000\004\000\005\000\000\000\000\000\
\000\000\015\000\000\000\000\000\011\000\000\000\000\000\017\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\067\000\
\053\000\054\000\055\000\056\000\057\000\000\000\061\000\000\000\
\000\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\020\000\000\000\072\000\023\000\
\000\000\029\000\030\000\032\000\000\000\000\000\040\000\000\000\
\000\000\000\000\000\000\007\000\062\000\063\000\064\000\016\000\
\012\000\027\000\000\000\000\000\066\000\058\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\073\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\065\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000\000\000\033\000\034\000\035\000\000\000\
\000\000\044\000\042\000\043\000\041\000\000\000\000\000\000\000\
\000\000\028\000\000\000\070\000\000\000\024\000\031\000\000\000\
\000\000\000\000\000\000\069\000\000\000\000\000\000\000\022\000\
\071\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\007\000\008\000\009\000\010\000\019\000\
\031\000\022\000\023\000\020\000\056\000\074\000\058\000\075\000\
\076\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\032\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\041\000"

let yysindex = "\020\000\
\021\255\000\000\027\255\000\000\000\000\001\000\000\000\015\255\
\000\000\023\255\036\255\000\000\000\000\000\000\070\255\070\255\
\079\255\000\000\069\255\063\255\000\000\074\255\071\255\000\000\
\019\255\069\255\077\255\108\255\109\255\166\255\084\255\000\000\
\000\000\000\000\000\000\000\000\000\000\069\255\000\000\112\255\
\123\255\070\255\000\000\070\255\041\255\041\255\127\255\041\255\
\041\255\140\255\134\255\000\000\000\000\009\000\000\000\000\000\
\126\255\000\000\000\000\000\000\045\255\004\255\000\000\087\255\
\249\254\115\255\120\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\135\255\137\255\000\000\000\000\143\255\144\255\
\154\255\156\255\041\255\168\255\253\254\000\000\041\255\041\255\
\041\255\041\255\041\255\041\255\041\255\041\255\041\255\041\255\
\041\255\041\255\041\255\000\000\041\255\069\255\069\255\041\255\
\169\255\041\255\000\000\041\255\000\000\000\000\000\000\045\255\
\045\255\000\000\000\000\000\000\000\000\087\255\087\255\249\254\
\115\255\000\000\161\255\000\000\167\255\000\000\000\000\176\255\
\069\255\140\255\041\255\000\000\183\255\184\255\069\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\180\255\000\000\000\000\000\000\054\255\186\255\
\000\000\000\000\194\255\000\000\000\000\000\000\197\255\000\000\
\000\000\194\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\196\255\000\000\000\000\
\000\000\152\255\000\000\000\000\203\255\000\000\000\000\000\000\
\000\000\000\000\032\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\078\255\174\255\000\000\252\254\
\222\255\239\255\010\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\204\255\000\000\000\000\000\000\000\000\
\000\000\000\000\203\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\105\255\
\132\255\000\000\000\000\000\000\000\000\195\255\205\255\230\255\
\247\255\000\000\165\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\211\000\246\255\000\000\000\000\000\000\
\192\000\000\000\000\000\000\000\000\000\226\255\000\000\144\000\
\000\000\000\000\011\000\234\255\000\000\032\000\224\255\130\000\
\131\000\000\000\157\255\000\000\000\000\000\000\000\000\000\000\
\000\000\193\000\208\255\000\000"

let yytablesize = 284
let yytable = "\057\000\
\012\000\082\000\123\000\124\000\018\000\021\000\045\000\107\000\
\090\000\091\000\045\000\045\000\045\000\045\000\108\000\077\000\
\096\000\079\000\080\000\045\000\001\000\097\000\003\000\085\000\
\045\000\045\000\045\000\011\000\045\000\132\000\014\000\072\000\
\046\000\073\000\015\000\137\000\018\000\018\000\018\000\018\000\
\018\000\051\000\018\000\052\000\053\000\016\000\018\000\018\000\
\018\000\018\000\054\000\087\000\088\000\089\000\013\000\018\000\
\018\000\018\000\018\000\018\000\018\000\018\000\018\000\118\000\
\119\000\013\000\013\000\112\000\113\000\025\000\122\000\017\000\
\013\000\125\000\013\000\013\000\013\000\128\000\042\000\024\000\
\026\000\133\000\036\000\036\000\043\000\044\000\048\000\027\000\
\036\000\028\000\029\000\030\000\036\000\036\000\036\000\036\000\
\068\000\109\000\110\000\111\000\134\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\037\000\037\000\092\000\
\093\000\094\000\095\000\037\000\127\000\049\000\050\000\037\000\
\037\000\037\000\037\000\114\000\115\000\116\000\117\000\070\000\
\037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
\038\000\038\000\071\000\078\000\081\000\086\000\038\000\083\000\
\098\000\100\000\038\000\038\000\038\000\038\000\099\000\101\000\
\014\000\102\000\103\000\038\000\038\000\038\000\038\000\038\000\
\038\000\038\000\038\000\014\000\014\000\068\000\051\000\046\000\
\052\000\053\000\014\000\104\000\014\000\014\000\014\000\054\000\
\068\000\068\000\106\000\126\000\129\000\055\000\130\000\068\000\
\039\000\068\000\068\000\068\000\039\000\039\000\039\000\039\000\
\131\000\135\000\136\000\017\000\009\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\046\000\059\000\010\000\
\060\000\046\000\046\000\046\000\046\000\025\000\026\000\047\000\
\013\000\047\000\046\000\047\000\047\000\047\000\047\000\046\000\
\046\000\046\000\105\000\120\000\047\000\121\000\069\000\000\000\
\048\000\047\000\047\000\047\000\048\000\048\000\048\000\048\000\
\049\000\000\000\000\000\000\000\049\000\049\000\049\000\049\000\
\000\000\050\000\000\000\048\000\048\000\050\000\050\000\050\000\
\050\000\051\000\003\000\049\000\049\000\051\000\051\000\051\000\
\051\000\051\000\084\000\052\000\053\000\050\000\000\000\000\000\
\000\000\000\000\054\000\000\000\052\000\051\000\000\000\000\000\
\052\000\052\000\052\000\052\000"

let yycheck = "\030\000\
\000\000\050\000\102\000\103\000\015\000\016\000\011\001\011\001\
\005\001\006\001\015\001\016\001\017\001\018\001\018\001\046\000\
\024\001\048\000\049\000\024\001\001\000\029\001\002\001\054\000\
\029\001\030\001\031\001\001\001\010\001\129\000\016\001\042\000\
\014\001\044\000\012\001\135\000\005\001\006\001\007\001\008\001\
\009\001\001\001\011\001\003\001\004\001\010\001\015\001\016\001\
\017\001\018\001\010\001\007\001\008\001\009\001\001\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\096\000\
\097\000\012\001\013\001\090\000\091\000\001\001\101\000\002\001\
\019\001\104\000\021\001\022\001\023\001\108\000\016\001\001\001\
\012\001\130\000\005\001\006\001\011\001\015\001\010\001\019\001\
\011\001\021\001\022\001\023\001\015\001\016\001\017\001\018\001\
\013\001\087\000\088\000\089\000\131\000\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\005\001\006\001\025\001\
\026\001\027\001\028\001\011\001\106\000\010\001\010\001\015\001\
\016\001\017\001\018\001\092\000\093\000\094\000\095\000\016\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\005\001\006\001\016\001\013\001\001\001\016\001\011\001\010\001\
\030\001\011\001\015\001\016\001\017\001\018\001\031\001\015\001\
\001\001\011\001\011\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\012\001\013\001\001\001\001\001\014\001\
\003\001\004\001\019\001\016\001\021\001\022\001\023\001\010\001\
\012\001\013\001\011\001\011\001\020\001\016\001\016\001\019\001\
\011\001\021\001\022\001\023\001\015\001\016\001\017\001\018\001\
\017\001\011\001\011\001\016\001\011\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\011\001\013\001\011\001\
\013\001\015\001\016\001\017\001\018\001\011\001\011\001\011\001\
\006\000\026\000\024\001\015\001\016\001\017\001\018\001\029\001\
\030\001\031\001\083\000\098\000\024\001\099\000\038\000\255\255\
\011\001\029\001\030\001\031\001\015\001\016\001\017\001\018\001\
\011\001\255\255\255\255\255\255\015\001\016\001\017\001\018\001\
\255\255\011\001\255\255\030\001\031\001\015\001\016\001\017\001\
\018\001\011\001\002\001\030\001\031\001\015\001\016\001\017\001\
\018\001\001\001\002\001\003\001\004\001\031\001\255\255\255\255\
\255\255\255\255\010\001\255\255\011\001\031\001\255\255\255\255\
\015\001\016\001\017\001\018\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  EQ\000\
  COMMA\000\
  SEMICOLON\000\
  COLON\000\
  QMARK\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  RETURN\000\
  BCEQ\000\
  BCGE\000\
  BCGT\000\
  BCLE\000\
  BCLT\000\
  BCNE\000\
  BLAND\000\
  BLOR\000\
  EOF\000\
  "

let yynames_block = "\
  IDENTIFIER\000\
  TP\000\
  BCONSTANT\000\
  INTCONSTANT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 22 "parser.mly"
            ( _1 )
# 299 "parser.ml"
               : int Lang.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_or_fun_list) in
    Obj.repr(
# 27 "parser.mly"
  (Prog (fst _1, snd _1) )
# 306 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var_or_fun) in
    Obj.repr(
# 33 "parser.mly"
  ( _1 )
# 313 "parser.ml"
               : 'var_or_fun_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_or_fun_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_or_fun) in
    Obj.repr(
# 35 "parser.mly"
    ( (fst _1 @ fst _2, snd _1 @ snd _2) )
# 321 "parser.ml"
               : 'var_or_fun_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vardecl) in
    Obj.repr(
# 40 "parser.mly"
  ( ([_1], []) )
# 328 "parser.ml"
               : 'var_or_fun))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fundefn) in
    Obj.repr(
# 42 "parser.mly"
    ( ([], [_1]) )
# 335 "parser.ml"
               : 'var_or_fun))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'fundecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'vardecl_semi_list_opt) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'block_item_list_opt) in
    Obj.repr(
# 47 "parser.mly"
  ( Fundefn(_1, _3, _4) )
# 344 "parser.ml"
               : 'fundefn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Lang.tp) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'vardecl_comma_list_opt) in
    Obj.repr(
# 51 "parser.mly"
  ( Fundecl(_1, _2, _4) )
# 353 "parser.ml"
               : 'fundecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
  ( [] )
# 359 "parser.ml"
               : 'vardecl_comma_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl_comma_list) in
    Obj.repr(
# 58 "parser.mly"
    ( _1 )
# 366 "parser.ml"
               : 'vardecl_comma_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 64 "parser.mly"
    ( [_1] )
# 373 "parser.ml"
               : 'vardecl_comma_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vardecl_comma_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 66 "parser.mly"
    ( _1 @ [_3] )
# 381 "parser.ml"
               : 'vardecl_comma_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
  ( [] )
# 387 "parser.ml"
               : 'vardecl_semi_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vardecl_semi_list) in
    Obj.repr(
# 73 "parser.mly"
    ( _1 )
# 394 "parser.ml"
               : 'vardecl_semi_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 79 "parser.mly"
    ( [_1] )
# 401 "parser.ml"
               : 'vardecl_semi_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vardecl_semi_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl) in
    Obj.repr(
# 81 "parser.mly"
    ( _1 @ [_3] )
# 409 "parser.ml"
               : 'vardecl_semi_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lang.tp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
    ( Vardecl(_1, _2) )
# 417 "parser.ml"
               : 'vardecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parser.mly"
    ( VarE(0, Var(Local, _1)) )
# 424 "parser.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 96 "parser.mly"
    ( Const(0, BoolV _1) )
# 431 "parser.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 98 "parser.mly"
    ( Const(0, IntV _1) )
# 438 "parser.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 101 "parser.mly"
    ( _2 )
# 445 "parser.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 103 "parser.mly"
    ( IfThenElse (0, _2, _4, _6) )
# 454 "parser.ml"
               : 'primary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary_expression) in
    Obj.repr(
# 109 "parser.mly"
      ( _1 )
# 461 "parser.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'argument_expression_list_opt) in
    Obj.repr(
# 111 "parser.mly"
      ( CallE(0, _1, _3) )
# 469 "parser.ml"
               : 'postfix_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
    ( [] )
# 475 "parser.ml"
               : 'argument_expression_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argument_expression_list) in
    Obj.repr(
# 118 "parser.mly"
      ( _1 )
# 482 "parser.ml"
               : 'argument_expression_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 123 "parser.mly"
      ( [_1] )
# 489 "parser.ml"
               : 'argument_expression_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argument_expression_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 125 "parser.mly"
      ( _1 @ [_3] )
# 497 "parser.ml"
               : 'argument_expression_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'postfix_expression) in
    Obj.repr(
# 130 "parser.mly"
      ( _1 )
# 504 "parser.ml"
               : 'unary_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary_expression) in
    Obj.repr(
# 136 "parser.mly"
      ( _1 )
# 511 "parser.ml"
               : 'cast_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Lang.tp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expression) in
    Obj.repr(
# 138 "parser.mly"
      ( _4 )
# 519 "parser.ml"
               : 'cast_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expression) in
    Obj.repr(
# 143 "parser.mly"
      ( _1 )
# 526 "parser.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'multiplicative_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expression) in
    Obj.repr(
# 145 "parser.mly"
    ( BinOp(0, BArith BAmul, _1, _3) )
# 534 "parser.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'multiplicative_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expression) in
    Obj.repr(
# 147 "parser.mly"
    ( BinOp(0, BArith BAdiv, _1, _3) )
# 542 "parser.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'multiplicative_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cast_expression) in
    Obj.repr(
# 149 "parser.mly"
    ( BinOp(0, BArith BAmod, _1, _3) )
# 550 "parser.ml"
               : 'multiplicative_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'multiplicative_expression) in
    Obj.repr(
# 154 "parser.mly"
    ( _1 )
# 557 "parser.ml"
               : 'additive_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'additive_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'multiplicative_expression) in
    Obj.repr(
# 156 "parser.mly"
    ( BinOp(0, BArith BAadd, _1, _3) )
# 565 "parser.ml"
               : 'additive_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'additive_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'multiplicative_expression) in
    Obj.repr(
# 158 "parser.mly"
    ( BinOp(0, BArith BAsub, _1, _3) )
# 573 "parser.ml"
               : 'additive_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'additive_expression) in
    Obj.repr(
# 163 "parser.mly"
    ( _1 )
# 580 "parser.ml"
               : 'shift_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 168 "parser.mly"
    ( _1 )
# 587 "parser.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 170 "parser.mly"
    ( BinOp(0, BCompar BClt, _1, _3) )
# 595 "parser.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 172 "parser.mly"
    ( BinOp(0, BCompar BCgt, _1, _3) )
# 603 "parser.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 174 "parser.mly"
    ( BinOp(0, BCompar BCle, _1, _3) )
# 611 "parser.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'relational_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'shift_expression) in
    Obj.repr(
# 176 "parser.mly"
    ( BinOp(0, BCompar BCge, _1, _3) )
# 619 "parser.ml"
               : 'relational_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expression) in
    Obj.repr(
# 181 "parser.mly"
    ( _1 )
# 626 "parser.ml"
               : 'equality_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equality_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expression) in
    Obj.repr(
# 183 "parser.mly"
    ( BinOp(0, BCompar BCeq, _1, _3) )
# 634 "parser.ml"
               : 'equality_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equality_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'relational_expression) in
    Obj.repr(
# 185 "parser.mly"
    ( BinOp(0, BCompar BCne, _1, _3) )
# 642 "parser.ml"
               : 'equality_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'equality_expression) in
    Obj.repr(
# 190 "parser.mly"
    ( _1 )
# 649 "parser.ml"
               : 'logical_AND_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'logical_AND_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'equality_expression) in
    Obj.repr(
# 192 "parser.mly"
    ( IfThenElse(0, _1, _3, Const(0, BoolV false)) )
# 657 "parser.ml"
               : 'logical_AND_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logical_AND_expression) in
    Obj.repr(
# 202 "parser.mly"
    ( _1 )
# 664 "parser.ml"
               : 'logical_OR_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'logical_OR_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'logical_AND_expression) in
    Obj.repr(
# 204 "parser.mly"
    ( IfThenElse(0, _1, Const(0, BoolV true), _3) )
# 672 "parser.ml"
               : 'logical_OR_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logical_OR_expression) in
    Obj.repr(
# 213 "parser.mly"
    ( _1 )
# 679 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_statement) in
    Obj.repr(
# 221 "parser.mly"
                       ( _1 )
# 686 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression_statement) in
    Obj.repr(
# 222 "parser.mly"
                         ( _1 )
# 693 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'selection_statement) in
    Obj.repr(
# 223 "parser.mly"
                         ( _1 )
# 700 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'iteration_statement) in
    Obj.repr(
# 224 "parser.mly"
                        ( _1 )
# 707 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'jump_statement) in
    Obj.repr(
# 225 "parser.mly"
                   ( _1 )
# 714 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'block_item_list_opt) in
    Obj.repr(
# 232 "parser.mly"
    ( _2 )
# 721 "parser.ml"
               : 'compound_statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 237 "parser.mly"
  ( Skip)
# 727 "parser.ml"
               : 'block_item_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block_item_list) in
    Obj.repr(
# 239 "parser.mly"
    ( _1 )
# 734 "parser.ml"
               : 'block_item_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block_item) in
    Obj.repr(
# 244 "parser.mly"
    ( _1 )
# 741 "parser.ml"
               : 'block_item_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block_item_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block_item) in
    Obj.repr(
# 246 "parser.mly"
    ( Seq (_1, _2) )
# 749 "parser.ml"
               : 'block_item_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'assignment) in
    Obj.repr(
# 252 "parser.mly"
                         ( _1 )
# 756 "parser.ml"
               : 'expression_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'call_statement) in
    Obj.repr(
# 253 "parser.mly"
                             ( _1 )
# 763 "parser.ml"
               : 'expression_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'argument_expression_list_opt) in
    Obj.repr(
# 259 "parser.mly"
    ( CallC(_1, _3) )
# 771 "parser.ml"
               : 'call_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 267 "parser.mly"
  ( Assign(0, Var(Local, _1), _3) )
# 779 "parser.ml"
               : 'assignment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 277 "parser.mly"
                       ( _1 )
# 786 "parser.ml"
               : 'block_item))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 283 "parser.mly"
  ( Cond(_3, _5, Skip) )
# 794 "parser.ml"
               : 'selection_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 285 "parser.mly"
    ( Cond(_3, _5, _7) )
# 803 "parser.ml"
               : 'selection_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 291 "parser.mly"
  ( While(_3, _5) )
# 811 "parser.ml"
               : 'iteration_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'assignment) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'assignment) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 296 "parser.mly"
    ( Seq (_3, While (_5, Seq (_9, _7))) )
# 821 "parser.ml"
               : 'iteration_statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 302 "parser.mly"
  ( Return (Const (0, VoidV)) )
# 827 "parser.ml"
               : 'jump_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 304 "parser.mly"
    ( Return _2 )
# 834 "parser.ml"
               : 'jump_statement))
(* Entry start *)
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
let start (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : int Lang.prog)
