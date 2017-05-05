(* Analyses of source language statements:
 - predicate: 'statement returns'
 - depth of operand stack for expression evaluation
 - definite assignment
*)

open Lang
open List

(* ************************************************************ *)
(* ****  Statement returns                                 **** *)
(* ************************************************************ *)

let rec stmt_returns = function
  | (Skip) -> false
  | (Assign (_,_,_)) -> false
  | (Seq (s1,s2)) -> (stmt_returns s1) || (stmt_returns s2)
  | (Cond (_,s1,s2)) -> (stmt_returns s1) || (stmt_returns s2)
  | (While (_,s)) -> (stmt_returns s)
  | (CallC (_,_)) -> false
  | (Return (e)) -> true;;



(* ************************************************************ *)
(* ****  Stack depth                                       **** *)
(* ************************************************************ *)

(**
* unlist
* Take a list of lists and make a list of elements in the lists
*
**)
exception AnElementIsAnEmptyList;;
let rec unlist = function
	((a::l1)::l2) -> a::(unlist l2)
	|[] -> []
	| _ -> raise AnElementIsAnEmptyList;;

(**
* addlist
* Take a list and add all of its elements.
*
**)
let rec addlist = function
	(a::l) -> a + (addlist l)
	|[] -> 0;;

(**
* maximum
* Find the max of two values.
*
**)
let maximum = function
	(a,b) -> if a > b then a else b;;

(**
* maximumList
* Find the max in a list.
*
**)
exception EmptyList;;

let rec maximumList = function
	(a::b::l) -> if a > b then (maximumList (a::l)) else (maximumList (b::l))
  |[a] -> a
  |[] -> raise EmptyList;;

let rec stack_depth_e = function
  Const (_, _) -> 1
  | VarE (_, _) -> 1
  | BinOp (_, _, expr1, VarE (_, _)) ->
    1 + (stack_depth_e expr1) + 1

  | BinOp (_, _, VarE (_, _), expr1) ->
    1 + (stack_depth_e expr1) + 1

  | BinOp (_, _, expr1, expr2) ->
    1 + (maximum ((stack_depth_e expr1) , (stack_depth_e expr2)))

  | IfThenElse (_, _,expr1,expr2) ->
    1 + maximum ((stack_depth_e expr1) ,(stack_depth_e expr2))

  | CallE (t, _,exprList) -> 1 + (maximumList(map stack_depth_e exprList));;

let rec stack_depth_c = function
  Skip -> 1
  | Assign (tp,var,expr) ->
    1 + (stack_depth_e expr)
  | Seq (stmt1, stmt2) ->
    maximum(stack_depth_c stmt1, stack_depth_c stmt2)
  | Cond (expr, stmt1, stmt2) ->
    maximum(stack_depth_e expr,maximum(stack_depth_c stmt1, stack_depth_c stmt2))
  | While (expr, stmt) ->
    maximum(stack_depth_e expr, stack_depth_c stmt)

  | CallC (fname, listOfExpr) ->
    1 + (addlist (map stack_depth_e listOfExpr))

  | Return (expr) ->
    1 + stack_depth_e expr;;



(* ************************************************************ *)
(* ****  Definite Assignment                               **** *)
(* ************************************************************ *)

module StringSet =
  Set.Make
    (struct type t = string
	    let compare = Pervasives.compare
     end)

(**
 stringSetMap
 The map function for a StringSet

 @author Romain ROCH
 @param env 'environment'
 @param stmt 'a stmt'
 @raise AssignTypeOfVarNotEqualToTypeOfExpression In an Assign, if the type
 of the expression isn't the same as the type of the 'Var'.
 @raise ConditionNotBoolean If the expression from the 'Cond' isn't from the
 'BoolT' type.
 @raise ReturnNotGood If the function find a return that is not of the
 'returntp' type from the given environment.
 @return A typed ''a stmt' from 'lang.ml'.
 @see 'lang.ml' This is the file where the types ''a stmt' and ''a expr'
 comes from.
 @since 0.9
 @version 0.9
*)
let rec stringSetMap func = function
  |(e::l) -> (func e)::(stringSetMap func l)
  |[] -> [];;

(**
* checkFalse
* Check the false in a list.
**)
let rec checkFalse = function
 (a::l) -> a != false && checkFalse l
 |[] -> false;;

let rec defassign_e vs = function
  Const (_,_) ->
    true
  | VarE (_,Var(binding,vname)) ->
    (StringSet.mem vname vs)
  | BinOp (_,_, expr1, expr2) ->
    (defassign_e vs expr1)
    && (defassign_e vs expr2)

  | IfThenElse (tp, expr1, expr2, expr3) ->
    (defassign_e vs expr1)
    && (defassign_e vs expr2)
    && (defassign_e vs expr3)

  | CallE (tp, fname, listOfExpr) ->
    checkFalse (stringSetMap (defassign_e vs) listOfExpr);;

exception AVariableIsntDefinitelyAssigned;;

let rec defassign_c allvs = function
  Skip -> []
  | Assign (tp, var, expr) ->
    if (defassign_e allvs expr)
    then [var]
    else raise AVariableIsntDefinitelyAssigned

  | Seq (stmt1, stmt2) ->
    (defassign_c allvs stmt1)@(defassign_c allvs stmt2)

  | Cond (expr, stmt1, stmt2) ->
    if (defassign_e allvs expr)
    then (defassign_c allvs stmt1)@(defassign_c allvs stmt2)
    else raise AVariableIsntDefinitelyAssigned

  | While (expr, stmt) ->
    if (defassign_e allvs expr)
    then (defassign_c allvs stmt)
    else raise AVariableIsntDefinitelyAssigned

  | CallC (fname, listOfExpr) ->
    if checkFalse (map (defassign_e allvs) listOfExpr)
    then []
    else raise AVariableIsntDefinitelyAssigned
  | Return (expr) ->
    if (defassign_e allvs expr)
    then []
    else raise AVariableIsntDefinitelyAssigned;;
