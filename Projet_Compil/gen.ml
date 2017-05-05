(* Compilation functions *)

open Lang
open Analyses
open Instrs
open Typing;; (* For tests *) (* Don't work, dunno why *)
open List

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

exception NotABinaryExpression;;
exception TheBinaryOperationIsNotAComparison;;
(**
* whatExpr
* Find if the expr is a comparison and what comparison.
**)
let rec whatExpr = function
  | BinOp (_,binOp,_,_) -> (match binOp with
                            BCompar(bCompar) -> (match bCompar with
                                                  BCeq -> BCeq
                                                  |BCge -> BCge
                                                  |BCgt -> BCgt
                                                  |BCle -> BCle
                                                  |BClt -> BClt
                                                  |BCne -> BCne)
                            |_ -> raise TheBinaryOperationIsNotAComparison)
  | _ -> raise NotABinaryExpression;;


(* gen_expr generate the "java bytecode" expression *)
let rec gen_expr varList l = function
    Const (t,v) -> [Loadc (t,v)]
    | VarE (t,v) -> [Loadv (t,(position (VarE (t,v)) varList)+1)]
    | BinOp (t,binOp,exp1,exp2) -> (gen_expr varList (1::l) exp1)
                                   @(gen_expr varList (2::l) exp2)
                                   @[Bininst(t,binOp)]
    | IfThenElse (t,e1,e2,e3) -> [If(whatExpr e1,(1::l))] (* IF *)
                                 @(gen_expr varList (2::l) e2) (* THEN *)
                                 @[Goto((4::l))] (* GOTO AFTER ELSE *)
                                 @[Label((1::l))] (* ELSE LABEL *)
                                 @(gen_expr varList (3::l) e3) (* ELSE *)
                                 @[Label((4::l))] (* IGNORE ELSE *)
    | CallE (t,nameF,exprList) ->
			[Invoke (t,nameF,(map tp_of_expr exprList))];;

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

exception CantAssignAnotherThingThanAConst;;
(* gen_stmt generate the "java bytecode" expression *)
let rec gen_stmt varList l = function
  Skip -> [Nop]

  | Assign (tp,Var(bind,nameVar),expr) ->
    (match expr with
      Const (t,c) ->
        [Loadc (t,c)]
        @[Storev(t,(position (Var(bind,nameVar)) varList)+1)] (* Storev always store in the first register (TO CHANGE)*)
			|_ -> raise CantAssignAnotherThingThanAConst)

      (* |VarE (t,v) ->
        [Loadv (t,position (VarE (t,v)) varList)] 			(* load(t,p) *)
        @[Storev(t,position (VarE (t,v)) varList))]) *) (* store(t,p) *)

  | Seq (statmnt1,statmnt2) ->
    (gen_stmt varList (1::l) statmnt1)
    @(gen_stmt varList (2::l) statmnt2)

  | Cond (expr,statmnt1,statmnt2) ->
    [If(whatExpr expr,(1::l))] (* IF *)
    @(gen_stmt varList (2::l) statmnt1) (* THEN *)
    @[Goto((4::l))] (* GOTO AFTER ELSE *)
    @[Label((1::l))] (* ELSE LABEL *)
    @(gen_stmt varList (3::l) statmnt2) (* ELSE *)
    @[Label((4::l))] (* IGNORE ELSE *)

  | While (expr,stmt) ->
	    [Label (1::l)]
	    @[If(whatExpr expr,(2::l))]
	    @gen_stmt varList (3::l) stmt
			@[Goto (1::l)]

  | CallC (fname,listOfExpr) ->
		[Invoke (VoidT,fname,(map tp_of_expr listOfExpr))]

  | Return (expr) ->
		[ReturnI (tp_of_expr expr)];;


(**
* genParameters
*	Translate a vardecl list in multiple Storev.
**)
let rec genParameters varList = function
	((Vardecl(tp,vname))::l) ->
		(Storev(tp,(position vname varList)+1))::(genParameters varList l)
	|[] -> [];;

(**
* findTpOfParam
*	Find the types of a vardecl list.
**)
let rec findTpOfParam = function
	((Vardecl(tp,vname))::l) ->
		tp::(findTpOfParam l)
	|[] -> [];;

(**
* gen_fundefn
* Translate a fundef in bytecode.
**)
let rec gen_fundefn varList l = function
	Fundefn (funDecl,varDeclListDef (* local vars *),stmt) ->
		match funDecl with
			Fundecl(tp,fname,varDeclListDecl (* Parameters *)) ->
				let nbVar = (List.length varDeclListDef)
				in
					[Methdefn (Methdecl (tp,fname,(findTpOfParam varDeclListDecl)),
										 Methinfo (stack_depth_c stmt,nbVar),
										 (gen_stmt varList l stmt))]
					(* @(genParameters varList varDeclListDecl)
					@[Label (1::l)]
					@(gen_stmt varList l stmt) *);;


(* Here's the tests for gen_expr *)
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
			  (gen_expr ([(VarE (IntT, (Var (Local, "n"))));(VarE (IntT, (Var (Local, "k"))))]) [] (tp_expr env expr3)@[ReturnI IntT]))]);;
