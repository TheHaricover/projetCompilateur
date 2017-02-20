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
