(* Datatypes for Bytecode instructions, methods, programs *)
(* Numbers refer to the Sun JVM doc, 
   http://java.sun.com/docs/books/vmspec/2nd-edition/html/VMSpecTOC.doc.html
 *)

open Lang

type label = int list

type instr = 
    Nop                         (* do nothing *)
  | Loadv of tp * int   	(* 3.11.2 -- load variable *)
  | Storev of tp * int  	(* store variable *)
  | Loadc of tp * value		(* load constant *)

  | Bininst of tp * binop	(* 3.11.3 -- binary operation *)
      (* 3.11.4 (type conversion, currently not used) *)
  | Getstatic of tp * vname	(* 3.11.5 -- dynamic field access not used here *)
  | Putstatic of tp * vname
      (* 3.11.6 -- pop and dup instrs, not needed here *)

  | If of bcompar * label	(* 3.11.7 -- conditional branch; corresponds to if_icmp<cond> *)
  | Goto of label		(* unconditional branch *)

  | ReturnI of tp		(* 3.11.8 *)
  | Invoke of tp * fname * (tp list)		(* method invocation *)
  | Label of label


(* method declaration: return type, function name, parameter type list *)
type methdecl = Methdecl of tp * fname * (tp list)

(* method info: stack limit and locals limit *)
type methinfo = Methinfo of int * int

type methdefn = Methdefn of methdecl * methinfo * (instr list)

type jvm_prog = JVMProg of (vardecl list) * (methdefn list)

