
(* Main function and target of compilation in Makefile *)


let main () = 
  Interf.generate Sys.argv.(1) Sys.argv.(2);;

main();;

