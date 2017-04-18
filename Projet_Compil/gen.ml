(* Compilation functions *)

open Lang
open Analyses
open Instrs
open Typing (* For tests *)

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)



(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)

let gen_prog (Prog (gvds, fdfs)) =
  JVMProg ([],
           [Methdefn (Methdecl (IntT, "even", [IntT]),
                      Methinfo (3, 1),
                      [Loadc (IntT, IntV 0); ReturnI IntT])]);;


exception ElementNotFound;;

(* this function find the position of the element n and return it *)
let rec position n = function
    (a::l) -> if a = n
              then 0
              else 1 + (position n l)
    |[] -> raise ElementNotFound;;

(* genExpr generate the "java bytecode" expression *)
let rec genExpr varList = function
    Const (t,v) -> [Loadc (t,v)]
    | VarE (t,v) -> [Loadv (t,position (VarE (t,v)) varList)]
    | BinOp (t,binOp,exp1,exp2) -> (genExpr varList exp1)@(genExpr varList exp2)@[Bininst(t,binOp)]
    (*| IfThenElse (t, _, _, _) ->
    | CallE (t, _, _) -> *);;


(* Here's the tests for genExpr *)
(* If you have some hard times to make the tests work, then copy-paste all the "typing.ml" file in this file. *)
let env = {localvar = [("k", IntT);
                       ("n", IntT)];
           globalvar = [];
           returntp = VoidT;
           funbind = []};;


let expr3 = BinOp (0, BArith BAadd, VarE (0, Var (Local, "n")),
                      BinOp (0, BArith BAadd, VarE (0, Var (Local, "k")),
                                             Const (0, IntV 1)));;

let gen_prog (Prog (gvds, fdfs)) =
	JVMProg ([],
			 [Methdefn (Methdecl (IntT, "even", [IntT;IntT]),
			  Methinfo (5,5),
			  (genExpr ([(VarE (IntT, (Var (Local, "n"))));(VarE (IntT, (Var (Local, "k"))))]) (tp_expr env expr3)@[ReturnI IntT]))]);;
