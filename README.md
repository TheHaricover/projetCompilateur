# projetCompilateur

Premier problème rencontré : Dans la fonction "tp_expr" lors du filtrage de "BinOp", mon "if" testait le type des deux expressions utilisées lors de l'opération binaire mais aussi le type d'une des deux opérations avec le type "IntT" ou "BoolT". Cette deuxième partie de la condition de mon "if" obligeait les expressions entrées dans la fonction "tp_expr" a être de la forme :
					"BinOp of Lang.tp * binop * ('a expr) * ('a expr)" au lieu de
					"BinOp of   'a    * binop * ('a expr) * ('a expr)".

Pour régler ce problème j'ai créé la fonction "voidify" qui transforme les "0" (ou tout autre chose qu'il pourrait y avoir d'écrit) habituellement écrit en "VoidT", le type le plus neutre que j'ai trouvé. Peut être faudrait-il que je mette un type strictement neutre dans le type "tp".
Cette idée n'étant pas une excellente idée, et ayant obtenu de l'aide pour ce problème, j'ai opté pour une autre solution : créer des fonctions auxilliaires.



Second problème : Dans la fonction "tp_expr" lors du filtrage de "BinOp" ou de "IfThenElse", j'utilisais "tp_of_expr" du fichiers "lang.ml" implémenté via la commande "open". Seulement, cette commande semble ne pas fonctionner correctement sur mon ordinateur, j'ai alors été obligé d'utiliser "#use "lang.ml;;" au lieu de "open Lang". Il semblerai que le make n'accepte pas "#use "lang.ml;;". Donc j'ai copié-collé les fonctions dont je me sers et qui vienne de "lang.ml" dans "typing.ml".
