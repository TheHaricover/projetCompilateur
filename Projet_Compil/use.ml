#load "lang.cmo";;
#load "parser.cmo" ;;
#load "lexer.cmo" ;;
#load "analyses.cmo";;
#load "typing.cmo";;
#load "instrs.cmo";;
#load "gen.cmo";;
#load "print_instr.cmo";;
#load "interf.cmo";;

(* For using the parser:

- Evaluate this file (use.ml)
- open Interf ;;
- parse "Tests/even.c" ;;

* For code generation:

- Evaluate this file (use.ml)
- open Interf ;;
- generate "Tests/even.c" "Tests/Even.j";;

*)
