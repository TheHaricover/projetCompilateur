(* Typechecking of source programs *)

open Lang
open Analyses
open List


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

exception NoVariable;;
exception NoFunction;;
exception BadTypingBArith;;
exception BadTypingBCompar;;
exception BadTypingBLogic;;
exception BadTypingIf;;
exception BadTypingConstIsAVoidV;;
exception BadTypingVariableDoesntExists;;
exception BadTypingCallEVarListsNotEquals;;
exception BadTypingCallEExpressionNotOfTheGoodType;;

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
                                      in if typeExpr1 = typeExpr2
                                         && (typeExpr1 = IntT)
                                         then IntT
                                         else raise BadTypingBArith

    |(BCompar compOp,exp1tp,exp2tp) -> let typeExpr1 = (tp_of_expr (exp1tp))
									   and typeExpr2  = (tp_of_expr (exp2tp))
									   in
									   	if typeExpr1 = typeExpr2
										then BoolT
										else raise BadTypingBCompar

    |(BLogic logicOp,exp1tp,exp2tp) -> let typeExpr1 = (tp_of_expr (exp1tp))
                                       and typeExpr2  = (tp_of_expr (exp2tp))
                                       in
									   	if typeExpr1 = typeExpr2
										&& (typeExpr1 = BoolT)
										then BoolT
										else raise BadTypingBLogic;;

(* ifAux is here to determine if the if is well constructed and typed *)
let ifAux = function
    (exp1tp,exp2tp,exp3tp) -> let typeExpr1 = (tp_of_expr (exp1tp))
                              and typeExpr2  = (tp_of_expr (exp2tp))
                              and typeExpr3 = (tp_of_expr (exp3tp))
                              in
                                if typeExpr1 = BoolT
                                && typeExpr2 = typeExpr3
                                then typeExpr2
                                else raise BadTypingIf;;

(* foncAux returns the Fundecl associated to the function in the funbind of an environnement *)
let rec foncAux (nomF,env) = function
    ((Fundecl (typeF,fname,varL))::funbindL) -> (if fname = nomF
                                                 then Fundecl (typeF,fname,varL)
                                                 else (foncAux (nomF,env) funbindL))
    |[] -> raise NoFunction;;

(* tp_expr type the given "'a expr" *)
let rec tp_expr env = function
    (Const((_ : int),c)) -> (match c with
									(BoolV b) -> Const(BoolT,BoolV b)
									|(IntV i) -> Const(IntT,IntV i)
									|(VoidV) -> raise BadTypingConstIsAVoidV)

    |(VarE(_,Var(bind,name))) -> (try VarE(findTypeOfVar name (match bind
                                                               with
                                                                  Local -> env.localvar
                                                                  |Global -> env.globalvar),
                                           Var(bind,name))
                                  with
                                    NoVariable -> raise BadTypingVariableDoesntExists)

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
                                                                         else raise BadTypingCallEExpressionNotOfTheGoodType)
															|([],[]) -> []
															|([],_) -> raise BadTypingCallEVarListsNotEquals
															|(_,[]) -> raise BadTypingCallEVarListsNotEquals
                                 	in CallE (typeF,nomF,(compTypeAux (declVar,varLF)));;

(* "goodTypedList" verify that the types of all the expr in a list are equals *)
let rec goodTypedList = function
  (a::b::l) -> (tp_of_expr a) = (tp_of_expr b) && goodTypedList (b::l)
  |[a] -> true
  |[] -> true;;

exception ConditionNotBoolean;;
exception ReturnNotGood;;
exception ExpressionsNotEqualsInType;;

(* tp_stmt type the given "'a stmt" *)
let rec tp_stmt env = function
  | (Skip) -> Skip
  | (Assign (_,v,e)) -> let typedE = (tp_expr env e)
                        in (Assign (VoidT, v , typedE))
  | (Seq (s1,s2)) -> Seq ((tp_stmt env s1) , (tp_stmt env s2))
  | (Cond (e,s1,s2)) -> (let te = tp_expr env e
                        in (if tp_of_expr te = BoolT
                           then Cond (te,
                              (tp_stmt env s1),
                              (tp_stmt env s2))
                           else raise ConditionNotBoolean))
  | (While (e,s)) -> (let te = tp_expr env e
                        in (if tp_of_expr te = BoolT
                           then While(te, (tp_stmt env s))
                           else raise ConditionNotBoolean))
  | (CallC (nameF,eList)) -> (* "resp." let teList = (map (tp_expr env) eList)
                             in if goodTypedList teList
                                then *) CallC (nameF,
                                              (* "resp." teList *)
                                              (map (tp_expr env) eList))
                                (* "resp." else raise ExpressionsNotEqualsInType*)
  | (Return (e)) -> let te = tp_expr env e
                    in if tp_of_expr te = env.returntp
                       then Return (te)
                       else raise ReturnNotGood;;

(* let typeExprList = function
  (a::l) -> tp_of_expr a
  |[] -> ;;*)

let rec stopAuxilliareDoublons (a,a1) = function
  |((b,b1)::l) -> a1 != b1 && (stopAuxilliareDoublons (a,a1) l)
  |[] -> true;;

let rec stopAuxDoublons = function
  |((a,a1)::l) -> (stopAuxilliareDoublons (a,a1) l) && (stopAuxDoublons l)
  |[] -> true;;

let keywordsUnaccepted = ["auto";"break";"case";"char";"const";"continue";
                          "default";"do";"double";"else";"enum";"extern";
                          "float";"for";"goto";"if";"int";"long";"register";
                          "return";"short";"signed";"sizeof";"static";"struct";
                          "switch";"typedef";"union";"unsigned";"void";
                          "volatile";"while"];;

exception NoReturn;;

(**
* checkReturn
* Check the given statement and return "true" if there is a Return in the
* statement and "false" otherwise.
**)
let rec checkReturn = function
| (Skip) -> false
| (Assign (_,v,e)) -> false
| (Seq (s1,s2)) -> (checkReturn s1) || (checkReturn s2)
| (Cond (e,s1,s2)) -> (checkReturn s1) || (checkReturn s2)
| (While (e,s)) -> (checkReturn s)
| (CallC (nameF,eList)) -> false
| (Return (e)) -> true;;

(**
* checkReturn
* Check the given statement and return "true" if there is a Return in the
* statement and "false" otherwise.
**)
let rec checkKeywords keywords = function
  (a::l) -> (List.mem keywords a) && checkKeywords keywords
  |[] -> true;;

(* tp_fdefn verify the given "'a fundefn" *)
let rec tp_fdefn env = function
  Fundefn (funDecl,varDeclListDef,stmt) -> match funDecl
                                         with
                                          Fundecl (returnTp,nameF,varDeclListDecl) ->
                                                      if checkReturn stmt
                                                      then if (stopAuxDoublons varDeclListDecl)
                                                           and (checkKeywords keywordsUnaccepted varDeclListDecl)
                                                           
                                                      else raise NoReturn
