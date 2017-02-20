(* Analyses of source language statements:
 - predicate: 'statement returns'
 - depth of operand stack for expression evaluation
 - definite assignment
*)

open Lang

(* ************************************************************ *)
(* ****  Statement returns                                 **** *)
(* ************************************************************ *)

let rec stmt_returns = false;;



(* ************************************************************ *)
(* ****  Stack depth                                       **** *)
(* ************************************************************ *)



let rec stack_depth_e = 0;;

let rec stack_depth_c = 0;;



(* ************************************************************ *)
(* ****  Definite Assignment                               **** *)
(* ************************************************************ *)

module StringSet =
  Set.Make
    (struct type t = string
	    let compare = Pervasives.compare
     end);;

let rec defassign_e a = true;;

let rec defassign_c allvs a = a;;
