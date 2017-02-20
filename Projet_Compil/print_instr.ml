(* printing Bytecode instructions to Jasmin assembler *)

open Lang
open Instrs

let current_classname = "MyClass"
let small_indentation = "  "
let indentation = "     "


let pr_label pth = "lab" ^ (List.fold_right (fun p s ->  "_" ^ (string_of_int p)  ^ s) pth "")


let pr_tp_prefix = function
    BoolT -> failwith "BoolT: not a valid type prefix"
  | IntT  -> "i"
  | VoidT -> failwith "VoidT: not a valid type prefix"

let pr_type_descriptor = function
    BoolT -> failwith "BoolT: not a valid type descriptor"
  | IntT  -> "I"
  | VoidT -> "V"

let pr_field_name vn = current_classname^"/"^vn

let pr_bc_value = function
    BoolV b -> failwith "BoolV: not a valid bytecode value"
  | IntV i -> (string_of_int i)
  | VoidV -> failwith "VoidV: not a valid bytecode value"

let pr_return = function
    VoidT -> "return"
  | t -> (pr_tp_prefix t)^"return"

let pr_bcompar_suffix = function
    BCeq -> "eq"
  | BCge -> "ge"
  | BCgt -> "gt"
  | BCle -> "le"
  | BClt -> "lt"
  | BCne -> "ne"

let pr_barith t = function
    BAadd -> (pr_tp_prefix t)^"add"
  | BAsub -> (pr_tp_prefix t)^"sub"
  | BAmul -> (pr_tp_prefix t)^"mul"
  | BAdiv -> (pr_tp_prefix t)^"div"
  | BAmod -> (pr_tp_prefix t)^"rem"

let pr_blogic = function
    BLand -> "iand"
  | BLor  -> "ior"

let pr_bininst t = function 
    BArith b -> pr_barith t b
  | BCompar b -> failwith "no binary comparison instructions should be used"
  | BLogic b -> pr_blogic b

let pr_funname fn = current_classname^"/"^fn
let pr_argtps tps = "("^(List.fold_right (^) (List.map pr_type_descriptor tps) ")")

(* TODO: reconsider load instruction *)
(* TODO: Binary instructions *)

let pr_instr = function
    Nop          -> "nop"
  | Loadv (t, i) -> (pr_tp_prefix t)^"load "^(string_of_int i)
  | Storev (t, i) -> (pr_tp_prefix t)^"store "^(string_of_int i)

  | Loadc (t, vl) -> "sipush "^(pr_bc_value vl)

  | Bininst (t, bop) -> (pr_bininst t bop)

  | Getstatic (t, vn) -> "getstatic "^(pr_field_name vn)^" "^(pr_type_descriptor t)
  | Putstatic (t, vn) -> "putstatic "^(pr_field_name vn)^" "^(pr_type_descriptor t)


  | If (cmp, lb) -> "if_icmp"^(pr_bcompar_suffix cmp)^" "^(pr_label lb)
  | Goto lb -> "goto "^(pr_label lb)

  | ReturnI t -> pr_return t
  | Invoke (t, fn, tps) -> 
      "invokestatic "^(pr_funname fn)^(pr_argtps tps)^(pr_type_descriptor t)
  | Label lb -> (pr_label lb)^":"

let pr_methdecl (Methdecl (t, fn, tps)) = 
  ".method static "^fn^(pr_argtps tps)^(pr_type_descriptor t)^"\n"

let pr_methinfo (Methinfo (sts, ls)) = 
  small_indentation^".limit stack "^(string_of_int sts)^"\n"^
  small_indentation^".limit locals "^(string_of_int ls)

let rec pr_instrs pos = function
    [] -> "\n"
  | (Label lb) :: ins -> 
      "\n"^small_indentation^(pr_instr (Label lb))^(pr_instrs (pos + 1) ins)
  | i :: ins -> 
      "\n"^indentation^(pr_instr i)^(pr_instrs (pos + 1) ins)

let pr_methdefn (Methdefn (mdcl, minf, ins)) =
  (pr_methdecl mdcl)^(pr_methinfo minf)^(pr_instrs 0 ins)^".end method\n\n"

let pr_global_vardecl (Vardecl (t, vn)) = ".field static "^vn^" "^(pr_type_descriptor t)
let pr_global_vardecls vds = 
   (List.fold_right (^) (List.map pr_global_vardecl vds) "\n")

let class_prefix = 
  ".class "^current_classname^"\n"^
    ".super java/lang/Object\n\n"

let pr_jvm_prog (JVMProg (gvds, mdfs)) = 
  class_prefix^
  (pr_global_vardecls gvds)^
    (List.fold_right (^) (List.map pr_methdefn mdfs) "")
