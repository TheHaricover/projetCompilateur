(* Compilation functions *)

open Lang
open Analyses
open Instrs

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


exception ElementNonTrouve;;

let rec position n = function
    (a::l) -> if a = n
              then 0
              else 1 + (position n l)
    |[] -> raise ElementNonTrouve;;


let rec genExpr varList = function
    Const (t,v) -> [Loadc (t,v)]
    | VarE (t,v) -> [Loadv (t,position v varList)]
    | BinOp (t,binOp,exp1,exp2) -> (genExpr varList exp1)@(genExpr varList exp2)@[Bininst(t,binOp)]
    (*| IfThenElse (t, _, _, _) ->
    | CallE (t, _, _) -> *);;
