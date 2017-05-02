Projet 2

# Rendu 3: La Fouine 2

## Mode d'emploi:

* 
* -i ou rien 	: lance l'interpreteur (comportement par default)
* -m -machine 	: lance le compilateur, puis execute le code
* -interm		: lance le compilateur et affiche le resultat
* -d -debug 	: lance l'option debug

## A propos du rendu 2:
* fix des fonctions anonymes qui marchent correctement (cf anonyme.ml)
* fix du parsing des fonctions a plusieurs arguments (cf badfun_fixed.ml)
* fix des "variables perdues" du rendue 2 qui disparaissaient quand on demandait leur valeur plusieurs fois d'affilee (cf lostvar_fixed.ml)

## A propos des commentaires sur le rendu 2:
* fix des bugs mentionnés (expliquer pourquoi l'ancien code ne marchait pas n'est pas possible/utile car nous avons tout repris en profondeur) les bugs en question sont dans les fichiers bug1.ml et bug2.ml.
* fix du format: on peut donner en argument un fichier a la place de le pipe dans le programme
(exple: ./fouine -i -d test.ml)
* a propos des opérations booleenes, c'etait en fait un choix d'en faire des operations renvoyant des entiers, pour que cela reste coherent avec le reste du code... comme cela marche bien on ne l'a pas changé. 



* fix du shift reduce conflict (juste une priorité manquante qui n'avait pas été push sur la dernière version)
* JD: fix du format (/!\ pas sur d'avoir fini, a retravailler...)
* JD: rendre le code propre (comment de merde qui trainent)
* VR: interpreter toujours une fonction avant de la push dans l'environnement
* 


## Fonctionnalites


