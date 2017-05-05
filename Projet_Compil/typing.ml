(* Typechecking of source programs *)

open Lang
open Analyses
open List


(* I copy-paste this function here because if we just "open Lang", the "make"
	command tells us that Lang is unbound or so. By putting this function that
	comes from "lang.ml", the "make" command is happy and let us compile.  *)

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


(**
  tp_prog
*)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)]);;

(* All my exceptions used in my functions *)
(** Used in findTypeOfVar when the function doesn't find the given variable in
the given environment *)
exception NoVariable;;

(** Used in foncAux when it doesn't find the name of a function in the given
funbind of an environment *)
exception NoFunction;;

(** Used in opBinAux when the expressions of the given binop (BArith here)
aren't good (badly typed) *)
exception BadTypingBArith;;

(** Used in opBinAux when the expressions of the given binop (BCompar here)
aren't good (badly typed) *)
exception BadTypingBCompar;;

(** Used in opBinAux when the expressions of the given binop (BLogic here)
aren't good (badly typed) *)
exception BadTypingBLogic;;

(** Used in ifAux if the given expr for the condition isn't a boolean *)
exception BadTypingIf;;

(** Used in tp_expr if the given expr (Const here) is a VoidV (which make no
sense) *)
exception BadTypingConstIsAVoidV;;

(** Used in tp_expr if the given expr (Var here) is not in the given environment
(It's a more precise exception when a NoVariable is catch) *)
exception BadTypingVariableDoesntExists;;

(** Used in tp_expr in compTypeAux when the two given lists (the one is the
list given by the CallE and the second is the list given with the fundecl found
in the environment thanks to the given name of the function in CallE) haven't
the same length.*)
exception BadTypingCallEVarListsNotEquals;;

(** Used in tp_expr in compTypeAux when the two given lists (the one is the
list given by the CallE and the second is the list given with the fundecl
found in the environment thanks to the given name of the function in CallE)
haven't the same type of var in the correct order (lists contains var. Those
var must have the same type and must be in the same order for the two list) *)
exception BadTypingCallEExpressionNotOfTheGoodType;;

(** Used in tp_stmt when the condition of a Cond isn't a boolean *)
exception ConditionNotBoolean;;

(** Used in tp_stmt when the type of a return isn't correct (isn't equal to
the type of returntp in the environment) *)
exception ReturnNotGood;;

(** Used in tp_stmt when the given var in an Assign isn't of the same type as
the expression (trying to put an integer constant in a boolean variable for
example)*)
exception AssignTypeOfVarNotEqualToTypeOfExpression;;

(** Used in tp_fdefn when a parameter or a local variable is a keyword of the
C language*)
exception AParameterOrLocalVarIsAKeyword;;

(** Used in tp_fdefn when a parameter or a local variable is typed as a VoidT *)
exception VoidVarFound;;

(* Used in tp_fdefn when duplicates are found in the concatenation of the
local vars, the global vars and the parameters of the given function *)
exception DuplicatesInParametersLocalVarsOrGlobalVars;;

(** Used in tp_fdefn when the name of the given function is found in the
concatenation of the local vars and the parameters of the given function *)
exception AParameterOrLocalVarIsTheNameOfTheFunction;;

(* Used in tp_fdefn when a return is found and it doesn't have the same type
as the returntp of the environment *)
exception ReturnTypesAreNotTheSame;;

(** Used in tp_prog when a function is badly defined *)
exception AFunctionIsBadlyDefined;;



(**
  findTypeOfVar
  This function find the type of the given variable in the given environment.
  @author Romain ROCH
  @param name A string. It's a name of a variable.
  @param "(((s : Lang.vname),(t : Lang.tp))::l)" A list that is either a
  localvar from an environment either a globalvar.
  @raise NoVariable If the list is empty, than there isn't the variable the
  user is looking for.
  @return A 'tp' from lang.ml. Can be 'BoolT' | 'IntT' | 'VoidT'
  @see 'lang.ml' This is the file where the type 'tp' comes from.
  @since 0.9
  @version 0.9
*)
let rec findTypeOfVar name = function
    (((s : Lang.vname),(t : Lang.tp))::l) -> if s = name
				  then t
				  else findTypeOfVar name l
    |[] -> raise NoVariable;;


(**
  opBinAux
  This function filter the different type of a binop and check if they're
  correctly defined (for example the expressions of a BArith should be
  integers).
  @author Romain ROCH
  @param binOp A ''a expr' from 'lang.ml'.
  @raise BadTypingBArith If the ''a expr' is a 'BArith' and the two expressions
  coming from the ''a expr' can be operate through a BArith'.
  @return A 'tp' from lang.ml. Can be 'BoolT' | 'IntT'
  @see 'lang.ml' This is the file where the type 'BinOp' comes from.
  @since 0.9
  @version 0.9
*)
let opBinAux = function
    |(BArith arithOp,exp1tp,exp2tp) -> let typeExpr1 = (tp_of_expr (exp1tp))
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


(**
  ifAux
  Find out if the condition of a IfThenElse is a boolean and return VoidT if it
  is.
  @author Romain ROCH
  @param exp1tp A ''a expr' from 'lang.ml'.
  @raise BadTypingIf If the ''a expr' isn't of the BoolT type.
  @return A 'tp' from lang.ml. Can be 'BoolT' | 'IntT'
  @see 'lang.ml' This is the file where the type 'BinOp' comes from.
  @since 0.9
  @version 0.9
*)
let ifAux = function
    |exp1tp -> let typeExpr1 = (tp_of_expr (exp1tp))
                              in
                                if typeExpr1 = BoolT
                                then VoidT
                                else raise BadTypingIf;;


(**
  foncAux
  Returns the Fundecl associated to the given name in the funbind of the
  given environnement.
  @author Romain ROCH
  @param nomF A name of a function.
  @param funbindL A funbind from an environnement.
  @raise NoFunction If the given name is the name of a function in the given
  funbind.
  @return A 'Fundecl' from lang.ml.
  @see 'lang.ml' This is the file where the type 'Fundecl' comes from.
  @since 0.9
  @version 0.9
*)
let rec foncAux nomF = function
    |((Fundecl (typeF,fname,varL))::funbindL) -> (if fname = nomF
                                                 then Fundecl (typeF,fname,varL)
                                                 else (foncAux nomF funbindL))
    |[] -> raise NoFunction;;


(**
  tp_expr
  Type the given "'a expr".
  @author Romain ROCH
  @param env An environnement
  @param ''a expr' An 'a expr from 'lang.ml'.
  @raise BadTypingConstIsAVoidV If the given ''a expr' is a constant with the
  'VoidV' type.
  @raise BadTypingVariableDoesntExists If the given ''a expr' is a variable
  which doesn't exist in the given environnement.
  @return A ''a expr' from 'lang.ml'.
  @see 'lang.ml' This is the file where the types ''a expr' and 'VoidV'
  comes from.
  @since 0.9
  @version 0.9
*)
let rec tp_expr env = function
    (Const((_ : int),c)) -> (match c with
									|(BoolV b) -> Const(BoolT,BoolV b)
									|(IntV i) -> Const(IntT,IntV i)
									|(VoidV) -> raise BadTypingConstIsAVoidV)

    |(VarE(_,Var(bind,name))) -> (try VarE(findTypeOfVar name (match bind with
                                                                Local -> env.localvar
                                                                |Global -> env.globalvar),
                                           Var(bind,name))
                                  with
                                    NoVariable -> raise BadTypingVariableDoesntExists)

    |(BinOp(_ ,opBin,exp1,exp2)) ->
      let exp1tp = (tp_expr env exp1)
      and exp2tp = (tp_expr env exp2)
			in
        (BinOp(opBinAux(opBin,exp1tp,exp2tp),opBin,exp1tp,exp2tp))

    |(IfThenElse (_,exp1,exp2,exp3)) ->
      (let exp1tp = (tp_expr env exp1)
       and exp2tp = (tp_expr env exp2)
       and exp3tp = (tp_expr env exp3)
       in
        IfThenElse((ifAux(exp1tp)),exp1tp,exp2tp,exp3tp))

    |(CallE (_,nomF,declVar)) ->
      let Fundecl (typeF,nameF,varLF) = (foncAux nomF (env.funbind))
      in
        (**
          compTypeAux
          Compare the types of the two var given through two lists.

          The two lists are two lists of variables. One is full of constants
          and the other one full of Vardecl (tp*vname). The first one are the
          values for the paramaters (Vardecl (tp*vname)) in the second one as a
          Vardecl doesn't contain a value.
          If the two variables have the same type, the value can be attached
          to the parameter.

          @author Romain ROCH
          @param (l1,l2) A couple of two lists. l1 is a 'a expr list'. l2 is a
          'Vardecl list'.
          @raise BadTypingCallEExpressionNotOfTheGoodType If the ''a expr' from
          l1 hasn't the same type as the Vardecl in l2.
          @raise BadTypingCallEVarListsNotEquals If the two given lists aren't
          of the same size.
          @return A typed ''a expr list' from 'lang.ml'.
          @see 'lang.ml' This is the file where the types ''a expr' comes from.
          @since 0.9
          @version 0.9
        *)
        let rec compTypeAux = function
    			|((e::l1),((Vardecl (tp,_))::l2)) ->
            let exp = (tp_expr env e)
            in let tpExp = (tp_of_expr exp)
               in (if tpExp = tp
                   then exp::compTypeAux(l1,l2)
                   else raise BadTypingCallEExpressionNotOfTheGoodType)
  				|([],[]) -> []
  				|([],_) -> raise BadTypingCallEVarListsNotEquals
  				|(_,[]) -> raise BadTypingCallEVarListsNotEquals
         in CallE (typeF,nomF,(compTypeAux (declVar,varLF)));;


(**
  goodTypedList

  Verify that the types of all the expr in a list are equals.

  @author Romain ROCH
  @param l 'a expr list
  @return boolean
  @see 'lang.ml' This is the file where the types ''a expr' comes from.
  @since 0.9
  @version 0.9
*)
let rec goodTypedList = function
  (a::b::l) -> (tp_of_expr a) = (tp_of_expr b) && goodTypedList (b::l)
  |[a] -> true
  |[] -> true;;


(**
  tp_stmt
  Type the given ''a stmt'.

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
let rec tp_stmt env = function
  | (Skip) -> Skip

  | (Assign (_,((Var(bind,nameV)) as v),e)) ->
    let typedE = (tp_expr env e)
      in let tpE = tp_of_expr(typedE)
  			 and tpVar = (findTypeOfVar nameV (env.localvar))
  			 in if tpVar = tpE
  					then (Assign (VoidT, v , typedE))
  					else raise AssignTypeOfVarNotEqualToTypeOfExpression

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

  | (CallC (nameF,eList)) ->
    CallC (nameF,(map (tp_expr env) eList))

  | (Return (e)) -> let te = tp_expr env e
                    in if tp_of_expr te = env.returntp
                       then Return (te)
                       else raise ReturnNotGood;;

(* let typeExprList = function
  (a::l) -> tp_of_expr a
  |[] -> ;;*)

let rec stopAuxilliareDoublons (a,a1) = function
  |((b,b1)::l) -> a1 != b1 && (stopAuxilliareDoublons (a,a1) l)
  |[] -> false;;

  (**
  * stopAuxDoublons
  * Check all the elements of a list and return "false" if there is no
  *)
let rec stopAuxDoublons = function
  |((a,a1)::l) -> (stopAuxilliareDoublons (a,a1) l) && (stopAuxDoublons l)
  |[] -> false;;

let keywordsUnaccepted = ["auto";"break";"case";"char";"const";"continue";
                          "default";"do";"double";"else";"enum";"extern";
                          "float";"for";"goto";"if";"int";"long";"register";
                          "return";"short";"signed";"sizeof";"static";"struct";
                          "switch";"typedef";"union";"unsigned";"void";
                          "volatile";"while"];;


(**
* typeOfReturn
* Check the given statement and return the type of the return found if there is
* a Return in the statement. The case without a return returns an exception.
*)
let rec typeOfReturn = function
| (Skip) -> []
| (Assign (_,_,_)) -> []
| (Seq (s1,s2)) -> (typeOfReturn s1)@(typeOfReturn s2)
| (Cond (_,s1,s2)) -> (typeOfReturn s1)@(typeOfReturn s2)
| (While (_,s)) -> (typeOfReturn s)
| (CallC (_,_)) -> []
| (Return (e)) -> (tp_of_expr e)::[];;

(**
* checkListElementsEqualsElement
* Check the given list and return true if all the types in the list are the same
* type of the given type
*)
let rec checkListElementsEqualsElement tp = function
  (a::l) -> a = tp && (checkListElementsEqualsElement tp l)
  |[] -> true;;

(**
* checkKeywords
* Check the given statement and return "true" if there is a keyword in the
* statement and "false" otherwise.
*)
let rec checkKeywords keywords = function
  ((vname,_)::l) -> (mem vname keywords) && (checkKeywords keywords l)
  |[] -> false;;

(**
* checkFunctionName
* Check if the name of the couples is not the given name (used to check if the
* parameters or the local vars aren't named like the name of the function)
*)
let rec checkFunctionName fname = function
  ((vname,_)::l) -> (fname = vname) && (checkFunctionName fname l)
  |[] -> false;;

(**
* reversify reverse all the (vname * tp) in (tp * vname) of a (vname * tp) list
*)
let rec reversify = function
	((Vardecl(varName,varType))::l) -> (varType,varName)::(reversify l)
	|[] -> [];;

(**
* noVoidVar check if a var have the VoidT type
*)
let rec noVoidVar = function
	((varName,varType)::l) -> (varType != VoidT) && (noVoidVar l)
	|[] -> true;;


(**
* tp_fdefn verify the given "'a fundefn"
*)
let rec tp_fdefn env = function
  Fundefn (funDecl,varDeclListDef (* local vars *),stmt) -> match funDecl with
    Fundecl (returnTp,nameF,varDeclListDecl (* parameters *)) ->
    	if (noVoidVar (reversify (varDeclListDef@varDeclListDecl)))
    	then (let envT  = ({localvar = (reversify varDeclListDef)@(env.localvar);
    											globalvar = (env.globalvar);
    											returntp = returnTp;
    											funbind = funDecl::(env.funbind)})
    			 and listTpOfReturnStmt = (typeOfReturn (tp_stmt env stmt))
    				 in (if (checkListElementsEqualsElement returnTp listTpOfReturnStmt)
                 && (length listTpOfReturnStmt != 0) (* The returns' type are the same *)
    						then if (stopAuxDoublons ((reversify varDeclListDecl)
                                          @envT.globalvar
                                          @envT.localvar)) (* Parameters/localvar/globalvar aren't duplicates of each other*)
    								 then raise DuplicatesInParametersLocalVarsOrGlobalVars
    								 else if (checkKeywords keywordsUnaccepted (reversify varDeclListDecl)) (* Parameters aren't a C-keyword *)
    											|| (checkKeywords keywordsUnaccepted (envT.localvar)) (* Local vars aren't a C-keyword *)
    											then raise AParameterOrLocalVarIsAKeyword
    											else if (checkFunctionName nameF (reversify varDeclListDecl)) (* Function's name not in parameters *)
    												   || (checkFunctionName nameF (envT.localvar)) (* Function's name not in local vars *)
    													 then raise AParameterOrLocalVarIsTheNameOfTheFunction
    													 else true
    					else raise ReturnTypesAreNotTheSame))
    	else raise VoidVarFound;;

(**
* megaCheckFunctionName
* Check if the name of the couples is not in the given names list  (used to
* check if the global vars aren't named like the name of a function in the prog)
*)
let rec megaCheckFunctionName varList = function
  (Fundefn (Fundecl (returnTp,
                     nameF,
                     varDeclListDecl (* parameters *)),
            varDeclListDef (* local vars *),
            stmt)
  ::l1)
    -> (checkFunctionName nameF varList) && (megaCheckFunctionName varList l1)
  |[] -> false;;

(**
* checkKeywordsFunction
* Check the given function list and return "true" if the name of a function is a
* keyword.
*)
let rec checkKeywordsFunction keywords = function
  (Fundefn (Fundecl (returnTp,
                     nameF,
                     varDeclListDecl (* parameters *)),
            varDeclListDef (* local vars *),
            stmt)
  ::l1) -> (mem nameF keywords) && (checkKeywordsFunction keywords l1)
  |[] -> false;;

(**
* varDeclListConst
* Build a list of varDecl for the given 'a fundefn list
*)
let rec varDeclListConst = function
  (Fundefn (fundecl,_ (* local vars *),_)::l1)
    -> fundecl::varDeclListConst l1
  |[] -> [];;

(**
* checkFalse
* Check the false in a list
*)
let rec checkFalse = function
  (a::l) -> a = false && checkFalse l
  |[] -> true;;

let tp_prog = function
  (Prog(globalVars,listDefFun))
    -> let env = ({localvar = [];
                   globalvar = (reversify globalVars);
                   returntp = VoidT;
                   funbind = varDeclListConst listDefFun})
       in if (checkFalse (map (tp_fdefn env) listDefFun))
          then raise AFunctionIsBadlyDefined
          else Prog(globalVars,listDefFun);;
