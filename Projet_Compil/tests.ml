(* Here are the tests ! *)

(* Loading project modules *)
#load "lang.cmo";;
#load "typing.cmo";;
open Typing
open Lang


let env = {localvar = [("k", IntT);("n", IntT)];
           globalvar = [];
           returntp = IntT;
           funbind = [Fundecl(IntT,"f",[Vardecl (IntT,"y");Vardecl (BoolT,"jacques")])]};;

(* Const *)
let expr1 = Const (0, BoolV true);;

let expr2 = Const (0, BoolV false);;

let expr3 = Const (0, IntV 0);;

let expr4 = Const (0, VoidV);;

(* VarE *)

let expr5 = VarE (0, Var (Local, "k"));;

let expr6 = VarE (0, Var (Local, "n"));;

let expr7 = VarE (0, Var (Global, "elan"));;

(* BinOp *)

(* BArith *)

let expr8 = BinOp (0, BArith BAadd, expr1, expr2);;

let expr9 = BinOp (0, BArith BAadd, expr3, expr3);;

let expr10 = BinOp (0, BArith BAsub, expr1, expr2);;

let expr11 = BinOp (0, BArith BAsub, expr3, expr3);;

let expr12 = BinOp (0, BArith BAmul, expr1, expr2);;

let expr13 = BinOp (0, BArith BAmul, expr3, expr3);;

let expr14 = BinOp (0, BArith BAdiv, expr1, expr2);;

let expr15 = BinOp (0, BArith BAdiv, expr3, expr3);;

let expr16 = BinOp (0, BArith BAmod, expr1, expr2);;

let expr17 = BinOp (0, BArith BAmod, expr3, expr3);;

(* BCompar *)

let expr18 = BinOp (0, BCompar BCeq, expr1, expr2);;

let expr19 = BinOp (0, BCompar BCeq, expr1, expr1);;

let expr20 = BinOp (0, BCompar BCeq, expr1, expr3);;

let expr21 = BinOp (0, BCompar BCeq, expr3, expr3);;

let expr22 = BinOp (0, BCompar BCge, expr1, expr2);;

let expr23 = BinOp (0, BCompar BCge, expr1, expr1);;

let expr24 = BinOp (0, BCompar BCge, expr1, expr3);;

let expr25 = BinOp (0, BCompar BCge, expr3, expr3);;

let expr26 = BinOp (0, BCompar BCgt, expr1, expr2);;

let expr27 = BinOp (0, BCompar BCgt, expr1, expr1);;

let expr28 = BinOp (0, BCompar BCgt, expr1, expr3);;

let expr29 = BinOp (0, BCompar BCgt, expr3, expr3);;

let expr30 = BinOp (0, BCompar BCle, expr1, expr2);;

let expr31 = BinOp (0, BCompar BCle, expr1, expr1);;

let expr32 = BinOp (0, BCompar BCle, expr1, expr3);;

let expr33 = BinOp (0, BCompar BCle, expr3, expr3);;

let expr34 = BinOp (0, BCompar BClt, expr1, expr2);;

let expr35 = BinOp (0, BCompar BClt, expr1, expr1);;

let expr36 = BinOp (0, BCompar BClt, expr1, expr3);;

let expr37 = BinOp (0, BCompar BClt, expr3, expr3);;

let expr38 = BinOp (0, BCompar BCne, expr1, expr2);;

let expr39 = BinOp (0, BCompar BCne, expr1, expr1);;

let expr40 = BinOp (0, BCompar BCne, expr1, expr3);;

let expr41 = BinOp (0, BCompar BCne, expr3, expr3);;

(* C keywords *)
(* ["auto";"break";"case";"char";"const";"continue";"default";"do";"double";"else";"enum";"extern";"float";"for";"goto";"if";"int";"long";"register";"return";"short";"signed";"sizeof";"static";"struct";"switch";"typedef";"union";"unsigned";"void";"volatile";"while"] *)

(* tp_expr env (expr1);;
tp_of_expr (tp_expr env (expr1));;

tp_expr env (expr2);;
tp_of_expr (tp_expr env (expr2));;

tp_expr env (expr3);;
tp_of_expr (tp_expr env (expr3));;

tp_expr env (expr4);;
tp_of_expr (tp_expr env (expr4));;

tp_expr env (expr5);;
tp_of_expr (tp_expr env (expr5));;

tp_expr env (expr6);;
tp_of_expr (tp_expr env (expr6));; *)

let stmt1 = Seq ((CallC ("JACQUES_POINCER",[expr5;expr6;expr1]),(Return (expr3))));;

tp_stmt env stmt1;;
