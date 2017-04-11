Projet 2

# Rendu 2: La Fouine !

## The merdz a faire (Julien)
* implementer fun x -> ...
* gerer les priorites d'operateurs (en particulier +,-,x)


## The merdz a faire (Vincent)
* verifier que quand une fonction non recursive s'apelle, ca plante

## Les merdes que j'ai faites:

* les fonctions doivent marcher
* j'ai rajoute les fonctions recursives avec le mot clé Recfun, a toi de voir si ca te suffit pour les implementer
* on a toujours qu'une opération l'addition (on rajoutera les autres merdes quand tout le reste marchera ca sera facile)
* on autorise maintenant des trucs du genre:
let f a b c = a + b + c in 
let n = 3 in 
f n 12 (let a = 3 in f 1 4 a) ;;

ce que ca veut dire est qu'un appel de fonction admet des parametres qui peuvent etre: 
une value
une id
un prog entre parenthèses

