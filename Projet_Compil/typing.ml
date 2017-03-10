(* Typechecking of source programs *)

open Lang
open Analyses


(* I copy-paste this function here because if we just "open Lang", the "make"
	command tells us that Lang is unbound or so. By putting this function that
	comes from "lang.ml", the "make" command is happy and let us compile.  *)

(* auxiliary function; extracts the type component of an expression *)
let tp_of_expr = function
    Const (t, _) -> t
  | VarE (t, _) -> t
  | BinOp (t, _, _, _) -> t
  | IfThenElse (t, _, _, _) -> t
  | CallE (t, _, _) -> t;;


(* Environments *)

type environment =
    {localvar: (vname * tp) list;
     globalvar: (vname * tp) list;
     returntp: tp;
     funbind: fundecl list};;


(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)]);;

(* Const of 'a * value                        (* constant *)
| VarE of 'a * var                (* variable *)
| BinOp of 'a * binop * ('a expr) * ('a expr)   (* binary operation *)
| IfThenElse of 'a * ('a expr) * ('a expr) * ('a expr) (* if - then - else *)
| CallE of 'a * fname * ('a expr list) *)

exception NoVariable;;
exception NoFunction;;
exception BadTyping;;


(* This function find the type of a variable given on parameter *)
let rec findTypeOfVar name = function
    ((s,t)::l) -> if s = name
				  then t
				  else findTypeOfVar name l
    |[] -> raise NoVariable;;

(* This function filter the different type of a binop *)
let opBinAux = function
    (BArith arithOp,exp1tp,exp2tp) -> let typeExpr1 = (tp_of_expr (exp1tp))
									  and typeExpr2  = (tp_of_expr (exp2tp))
									  in
									  	if typeExpr1 = typeExpr2
										&& (typeExpr1 = IntT)
										then IntT
										else raise BadTyping

    |(BCompar compOp,exp1tp,exp2tp) -> let typeExpr1 = (tp_of_expr (exp1tp))
									   and typeExpr2  = (tp_of_expr (exp2tp))
									   in
									   	if typeExpr1 = typeExpr2
										then BoolT
										else raise BadTyping

    |(BLogic logicOp,exp1tp,exp2tp) -> let typeExpr1 = (tp_of_expr (exp1tp))
                                       and typeExpr2  = (tp_of_expr (exp2tp))
                                       in
									   	if typeExpr1 = typeExpr2
										&& (typeExpr1 = BoolT)
										then BoolT
										else raise BadTyping;;

(* ifAux is here to determine if the if is well constructed and typed *)
let ifAux = function
    (exp1tp,exp2tp,exp3tp) -> let typeExpr1 = (tp_of_expr (exp1tp))
                              and typeExpr2  = (tp_of_expr (exp2tp))
                              and typeExpr3 = (tp_of_expr (exp3tp))
                              in
                                if typeExpr1 = BoolT
                                && typeExpr2 = typeExpr3
                                then typeExpr2
                                else raise BadTyping;;

(* foncAux returns the Fundecl associated to the function in the funbind of an environnement *)
let rec foncAux (nomF,env) = function
    ((Fundecl (typeF,fname,varL))::funbindL) -> (if fname = nomF
                                                 then Fundecl (typeF,fname,varL)
                                                 else (foncAux (nomF,env) funbindL))
    |[] -> raise NoFunction;;

(* tp_expr is the principal function. It type all the 'a expr available *)
let rec tp_expr env = function
    (Const((_ : int),c)) -> (match c with
									(BoolV b) -> Const(BoolT,BoolV b)
									|(IntV i) -> Const(IntT,IntV i)
									|(VoidV) -> raise BadTyping)

    |(VarE(_,Var(bind,name))) -> (try VarE(findTypeOfVar name (match bind with
                                                                        Local -> env.localvar
                                                                        |Global -> env.globalvar),Var(bind,name))
                                  with
                                    NoVariable -> raise BadTyping)

    |(BinOp(_ ,opBin,exp1,exp2)) -> let exp1tp = (tp_expr env exp1)
									and exp2tp = (tp_expr env exp2)
									in
										(BinOp(opBinAux(opBin,exp1tp,exp2tp),opBin,exp1tp,exp2tp))

    |(IfThenElse (_,exp1,exp2,exp3)) -> (let exp1tp = (tp_expr env exp1)
                                         and exp2tp = (tp_expr env exp2)
                                         and exp3tp = (tp_expr env exp3)
                                         in
                                            IfThenElse((ifAux(exp1tp,exp2tp,exp3tp)),exp1tp,exp2tp,exp3tp))

    |(CallE (_,nomF,declVar)) -> let Fundecl (typeF,nameF,varLF) = (foncAux (nomF,env) (env.funbind))
                                 in let rec compTypeAux = function
								 							((e::l1),((Vardecl (tp,_))::l2)) -> let exp = (tp_expr env e)
                                                                            in let tpExp = (tp_of_expr exp)
                                                                               in (if tpExp = tp
                                                                                  then exp::compTypeAux(l1,l2)
                                                                                  else raise BadTyping)
															|([],[]) -> []
															|([],_) -> raise BadTyping
															|(_,[]) -> raise BadTyping
                                 	in CallE (typeF,nomF,(compTypeAux (declVar,varLF)));;


(* Here are the tests ! *)

(* let env = {localvar = [("k", IntT);
                       ("n", IntT)];
           globalvar = [];
           returntp = VoidT;
           funbind = [Fundecl(IntT,"f",[Vardecl (IntT,"y");Vardecl (BoolT,"jacques")])]};;

let expr1 = BinOp (0, BCompar BCeq, VarE (0, Var (Local, "n")),
BinOp (0, BArith BAadd, VarE (0, Var (Local, "k")),
Const (0, IntV 1)));;

let expr2 = VarE (0, Var(Local,"n"));;

let expr3 = BinOp (0, BCompar BCeq, VarE (0, Var (Local, "n")),
BinOp (0, BArith BAadd, VarE (0, Var (Local, "k")),
Const (0, IntV 1)));;

let expr4 = VarE (0, Var(Local,"k"));;

let expr5 = IfThenElse (0,expr3,expr2,expr4);;

let expr6 = CallE (0,"f",[Const (0,IntV 2);Const (0,BoolV true)]);;

tp_expr env (expr1);;
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
