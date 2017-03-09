(* Typechecking of source programs *)

open Analyses












(* Definition of source language data structures *)

(* variable names *)
type vname = string;;

(* function names *)
type fname = string;;

type binding = Local | Global;;

(* binary arithmetic operators *)
type barith = BAadd | BAsub | BAmul | BAdiv | BAmod;;

(* binary logic operators *)
type blogic = BLand | BLor;;

(* binary comparison operators *)
type bcompar = BCeq | BCge | BCgt | BCle | BClt | BCne;;

(* binary operators, combining all of the above *)
type binop =
    BArith of barith
  | BCompar of bcompar
  | BLogic of blogic;;

type value =
    BoolV of bool
  | IntV of int
  | VoidV;;

type var = Var of binding * vname;;

(* expresssions.
   The type parameter 'a is instantiated during type inference *)
type 'a expr =
    Const of 'a * value                        (* constant *)
  | VarE of 'a * var                (* variable *)
  | BinOp of 'a * binop * ('a expr) * ('a expr)   (* binary operation *)
  | IfThenElse of 'a * ('a expr) * ('a expr) * ('a expr) (* if - then - else *)
  | CallE of 'a * fname * ('a expr list);;

(* auxiliary function; extracts the type component of an expression *)
let tp_of_expr = function
    Const (t, _) -> t
  | VarE (t, _) -> t
  | BinOp (t, _, _, _) -> t
  | IfThenElse (t, _, _, _) -> t
  | CallE (t, _, _) -> t;;

(* expresssions.
   The type parameter 'a is as for expresssions *)
type 'a stmt =
    Skip
  | Assign of 'a * var * ('a expr)
  | Seq of ('a stmt) * ('a stmt)
  | Cond of ('a expr) * ('a stmt) * ('a stmt)
  | While of ('a expr) * ('a stmt)
  | CallC of fname * ('a expr list)
  | Return of ('a expr);;


(* Types *)
type tp = BoolT | IntT | VoidT;;

let numeric_tp = function
    IntT -> true
  | t -> false;;

(* variable / parameter declaration *)
type vardecl =
    Vardecl of tp * vname;;

let tp_of_vardecl (Vardecl (t, _)) = t;;

let name_of_vardecl (Vardecl (_, vn)) = vn;;

(* function declaration: return type; parameter declarations *)
type fundecl =
    Fundecl of tp * fname * (vardecl list);;

let params_of_fundecl (Fundecl (t, fn, pds)) = pds;;

(* function definition: function declaration; local var decls; function body *)
type 'a fundefn =
    Fundefn of fundecl * (vardecl list) * ('a stmt);;

(* program: global variable declarations; function definitions *)
type 'a prog =
    Prog of (vardecl list) * ('a fundefn list);;


















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

exception PasDeVariable;;

exception MauvaisTypage;;


let rec findTypeOfVar name = function
    ((s,t)::l) -> if s = name then t else findTypeOfVar name l
    |[] -> raise PasDeVariable;;

let opBinAux = function
    (BArith arithOp,exp1tp,exp2tp) -> let typeExpr1 = (tp_of_expr (exp1tp))
                                      in let typeExpr2  = (tp_of_expr (exp2tp))
                                         in
                                         if typeExpr1 = typeExpr2
                                         && (typeExpr1 = IntT)
                                         then IntT
                                         else raise MauvaisTypage

    |(BCompar compOp,exp1tp,exp2tp) -> let typeExpr1 = (tp_of_expr (exp1tp))
                                       in let typeExpr2  = (tp_of_expr (exp2tp))
                                          in
                                          if typeExpr1 = typeExpr2
                                          then BoolT
                                          else raise MauvaisTypage

    |(BLogic logicOp,exp1tp,exp2tp) -> let typeExpr1 = (tp_of_expr (exp1tp))
                                       in let typeExpr2  = (tp_of_expr (exp2tp))
                                          in
                                          if typeExpr1 = typeExpr2
                                          && (typeExpr1 = BoolT)
                                          then BoolT
                                          else raise MauvaisTypage;;

let ifAux = function
    (exp1tp,exp2tp,exp3tp) -> let typeExpr1 = (tp_of_expr (exp1tp))
                              in let typeExpr2  = (tp_of_expr (exp2tp))
                                 in let typeExpr3 = (tp_of_expr (exp3tp))
                                    in
                                    if typeExpr1 = BoolT
                                    && typeExpr2 = typeExpr3
                                    then typeExpr2
                                    else raise MauvaisTypage;;

let rec findVar name = function
    ((varName,tpVar)::l) -> if varName = name
                  then true
                  else findVar name l
    |[] -> false;;

let rec findVarFoncAux env = function
    (Vardecl (tpVar,varName)::varL) -> ((findVar varName env.localvar) && (findVarFoncAux env varL))
    |[] -> false;;

exception FonctionNonExistante;;

let rec foncAux (nomF,env) = function
    ((Fundecl (typeF,fname,varL))::funbindL) -> (if fname = nomF
                                                 then (if (findVarFoncAux env varL)
                                                       then typeF
                                                       else raise MauvaisTypage)
                                                 else (foncAux (nomF,env) funbindL))
    |[] -> raise FonctionNonExistante;;

let rec tp_expr env = function
    (Const(_,c)) -> (match c with
                        (BoolV b) -> Const(BoolT,BoolV b)
                        |(IntV i) -> Const(IntT,IntV i)
                        |(VoidV) -> Const(VoidT,VoidV))

    |(VarE(_,Var(bind,name))) -> (try VarE(findTypeOfVar name env.localvar,Var(bind,name))
                                  with
                                    PasDeVariable -> raise MauvaisTypage)

    |(BinOp(_ ,opBin,exp1,exp2)) -> let exp1tp = (tp_expr env exp1)
                                    in let exp2tp = (tp_expr env exp2)
                                        in
                                            (BinOp(opBinAux(opBin,exp1tp,exp2tp),opBin,exp1tp,exp2tp))

    |(IfThenElse (_,exp1,exp2,exp3)) -> (try (let exp1tp = (tp_expr env exp1)
                                        and exp2tp = (tp_expr env exp2)
                                        and exp3tp = (tp_expr env exp3)
                                        in
                                            IfThenElse((ifAux(exp1tp,exp2tp,exp3tp)),exp1tp,exp2tp,exp3tp)) with
                                                                                                            MauvaisTypage -> raise MauvaisTypage)

    |(CallE (_,nomF,declVar)) -> CallE ((foncAux (nomF,env) (env.funbind)),nomF,declVar);;

let env = {localvar = [("k", IntT);
                       ("n", IntT)];
           globalvar = [];
           returntp = VoidT;
           funbind = []};;

let expr1 = BinOp (0, BCompar BCeq, VarE (0, Var (Local, "n")),
BinOp (0, BArith BAadd, VarE (0, Var (Local, "k")),
Const (0, IntV 1)));;

let expr2 = VarE (0, Var(Local,"n"));;

let expr3 = BinOp (0, BCompar BCeq, VarE (0, Var (Local, "n")),
BinOp (0, BArith BAadd, VarE (0, Var (Local, "k")),
Const (0, IntV 1)));;

let expr4 = VarE (0, Var(Local,"k"));;

let expr5 = IfThenElse (0,expr3,expr2,expr4);;

tp_expr env (expr5);;
tp_of_expr (tp_expr env (expr5));;
