# projetCompilateur

First encountered problem : In the "tp_expr" function, on the filter of "BinOp", my "if" was testing the type of the two used expressions by the binary operation but also the type of one of the two expressions with a "tp" type (like "IntT" or "BoolT"). This second part of my "if"'s condition was obliging the entered expressions in the "tp_expr" function to be of the form :
		"BinOp of Lang.tp * binop * ('a expr) * ('a expr)" instead of
		"BinOp of   'a    * binop * ('a expr) * ('a expr)".

To adjust this problem, I first created the "voidify" function, which transform "0" (or whatever it's write) in "VoidT", the most neutral type I found.

This idea wasn't good enough and from the help of some other people, I opted for another solution : create auxiliary functions.

The auxiliary functions didn't resist to the establishment of the "CallE" filter. Without any obvious reason. Because I couldn't find the reason I saw myself in the obligation of recode "voidify".

Finally, since "CallE", "voidify" don't work. I'm going to seek for another solution.

A very charming colleague helped me for "CallE", because I had an error which forbade me to enter non-typed expressions (with "0" instead of "tp") in "tp_expr". Thanks to his voodoo magic, probably, and thanks to my brain, him and I took down this problem and found a solution. The ultimate solution, realised by my colleague's hands who's a CAML professional.

Second problem : In the "tp_expr" function, on the filter of "BinOp" and "IfThenElse", I used "tp_of_expr" from the "lang.ml" file implemented through the command "open Lang". However, this command seems to not correctly work on my computer, so I was in the obligation to use "#use "lang.ml;;" instead of "open Lang".

It seems that "make" don't accept "#use "lang.ml;;". So I copy-pasted the functions which I use and which comes from "lang.ml" in "typing.ml".
