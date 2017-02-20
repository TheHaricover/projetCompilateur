(* Typechecking of source programs *)

open Lang
open Analyses

(* Environments *)

type environment =
    {localvar: (vname * tp) list;
     globalvar: (vname * tp) list;
     returntp: tp;
     funbind: fundecl list};;


(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;

(* Const of 'a * value                        (* constant *)
| VarE of 'a * var                (* variable *)
| BinOp of 'a * binop * ('a expr) * ('a expr)   (* binary operation *)
| IfThenElse of 'a * ('a expr) * ('a expr) * ('a expr) (* if - then - else *)
| CallE of 'a * fname * ('a expr list) *)

exception PasDeVariable;;

exception MauvaisTypage;;

let rec findVar name = function
    ((s,t)::l) -> if s = name then t else findVar name l
    |[] -> raise PasDeVariable;;

let rec tp_expr env = function
    Const(_,n) -> (match n with
                        (BoolV b) -> Const(BoolT b,BoolV b)
                        |(IntV i) -> Const(IntT i,IntV i)
                        |(VoidV) -> Const(VoidT,VoidV))
    |VarE(_,Var(bind,name)) -> (try VarE(findVar name env.localvar,Var(bind,name)) with
                                                        PasDeVariable -> raise MauvaisTypage)
    |BinOp(_,opBin,exp1,exp2) -> if tp_expr env exp1 = tp_expr env exp2 && tp_expr env exp1 =

      | BCompar of bcompar
      | BLogic of blogic)



;;
