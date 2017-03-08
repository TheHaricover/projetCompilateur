open Lang
open Analyses
open Typing

let env = environment ({localvar = [("k", IntT); ("n", IntT)]; globalvar = [];
returntp = VoidT; funbind = []});;

(tp_expr  (Const (0, IntV 1)));;
