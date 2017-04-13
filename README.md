Projet 2

# Rendu 2: La Fouine !

Note: comme pour le premier rendu, le parsing se fait a partir de stdin, donc il faut pipe les exemples dedans!

## Mode d'emploi

* sans aucun argument, fouine construit juste le prog.
* avec -i il interprete
* avec -d il dit a chaque etape ce qu'il fait

## Fonctionnalites
* somme soustraction multiplication et constantes y compris neg.
* fonctions: on peut declarer les fonctions avec
let f x y z = ... et fun x -> ... et les deux en meme temps ( cf exemple functions.in ) 
/!\ les fonctions anonymes sont parsées correctement mais pas interpretes (pb de cloture dans la manière dont on a code l'interpreteur)
/!\ il y a une erreur avec la notation fun x -> ... comme on peut le voir dans l'exemple functions.in avec les fonctions h et z:
en effet, let f x y z ... cree l'arbre dans un sens et fun x -> ... dans l'autre ce qui fait des contradictions dans le passage d'arguments. La bonne methode est donc d'utiliser let f x y z ... pour l'instant et cette coquille sera reparée pour le prochain rendu(cf badfun.bug).
/!\ quand on appelle une fonction avec une variable, cette derniere est supprimee de l'environement de base (je m'en suis rendu compte en faisant des tests " pas ridicules ". Ce sera aussi fix pour le prochain rendu. (cf lostvar.bug)
* la construction let ... in marche mais les suites de let ne sont pas implementes.
* fonctions recursives implementes si on essaye de la recursion sans fonction recursive ca plante ( les fonctions non rec. ne sont pas ajoutes a leur cloture ce qui les empeche de s'apeller eux meme)
* le parenthesage et les sauts de lignes sont correctement compris (cf goodparsing.in)
* les references sont implementes mais en l'absence de structures imperatives ca ne sert a rien...

## Testcases
* operators.in -> priority of operators test :  print > any fun > * > + = -
* fact.in		-> factorielle
* goodparsing.in	-> ...
* functions.in		-> test on functions
* reference.in		-> teste les refs
* lostvar.bug		-> pb de la variable qui disparait
* badfun.bug		-> probleme de non coherence entre les 2 manières de def les fonctions




