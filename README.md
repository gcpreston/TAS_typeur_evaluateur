# Typeur-Évaluateur

Le projet typeur-évaluateur pour TAS : Sorbonne Université M2 2025.

## La langue

Ce projet implémente un lexer, un parseur, un typeur, et un évaluateur pour une langue customisée. Elle suit la plupart des règles d'OCaml, et son syntaxe est similaire :

```
let id = fun x -> x in
id 5
```

Plusieurs exemples peuvent être trouvés dans le dossier `examples/`. Il faut noter qu'un programme est une seule expression.

Les fonctionnalités implémentées sont :
– numéros
– addition, soustraction
– listes
– fonctions, récursion par point fixe
– applications
– if zero, if empty
- let
– ref., deref, assign

## Build et exécution

### Typage

Pour trouver le type d'un programme dans un fichier :

```bash

dune exec type <path/to/program.tas>

# par exemble : dune exec typage examples/factorial.tas

```

### Évaluation

Pour évaluer un programme dans un fichier :

```bash

dune exec eval <path/to/program.tas>

# par exemble : dune exec typage examples/factorial.tas

```

### Tests

Pour exécuter les tests :

```bash

dune test

```

## Rapport

### État des lieux des fonctionnalités

De ma connaissance, tout ce qui est décrit dans le sujet marche dans cette implémentation : le lambda-calcul simplement typé, les entiers, les listes, let, let-polymorphisme, ref, deref, l'assignement, et le polymorphisme faible. Par contre, il n'y a pas d'extensions ; je n'avais pas le temps après de régler beaucoup de problèmes avec le let-polymorphisme et le polymorphisme faible, qui sont décrits dans la section suivante.

### Défis

#### Le polymorphisme

Le plus grand défi était avec le let-polymorphisme. Le grand problème était que chaque fois que je pensais que je l'avais fait correctement, la solution réglait un cas mais en cassait un autre. Par exemple :

```
# programme 1
let id = fun x -> x in
id 5

# programme 2
let id = fun x -> x in
let _ = id 5 in
id []
```

J'avais du mal à faire marcher au même temps. Le programme 1 avait soit le bon type, soit le mauvais type. Également, le programme 2 était entre le bon type et le mauvais résultat. Le problème était qu'en typant `id 5` comme un type général `T`, on permet de typer le programme 2, mais si on type `id 5` spécifiquement, on ne lui permet pas d'avoir un autre type plus tard. Effectivement, ce que cela veut dire est que le let-polymorphisme ne marche pas.

Le problème enfin était l'instantiation des schémas. Cela était fait dans la génération des équations, mais le résultat de cela est que les nouvelles variables de type ne sont pas liées l'une à l'autre. La solution est de faire l'instantiation dans l'unification : quand on rencontre un type `SchemeType` dans l'unification, faire une initialisation, et ensuite l'utiliser comme normal. Comme ça, on conserve les relations entre équations de la génération, en aussi différenciant entre plusieurs usages d'un seul schéma.

#### Polymorphisme faible

Le polymorphisme faible a aussi posé un défi de compréhension. Après avoir compris que l'idée de haut niveau est de simplement parfois éviter d'invoquer la généralisation du let-polymorphisme, il est devenu plus facile à comprendre.

#### Récursion

Le défi final était la réduction des opérateurs point fixe. J'ai rencontré des problèmes soit de loop infini quand je définis un point fixe dans un `let`, soit de ne pas évaluer une application au point fixe. Enfin l'erreur était que j'ai fait des réductions sous les lambdas pendant la définition d'une fonction.

#### Problèmes non résolus

Je n'ai pas encore trouvé une solution propre pour la généralisation de valeurs. Le problème est que l'endroit où la génération doit être faite est assez profond : il n'est pas facile de le faire après avoir reçu le résultat du subcall de `genere_equa` pour la branche `Let`. Pour cette raison, on généralise les valeurs `Abs`, quand on est dans un `Let`, mais seulement les valeurs `Abs`, donc c'est possible qu'il existe des cas où le let-polymorphisme et/ou le polymorphisme faible marchent pas comme prévu.

### Choix d’implémentation

– Gestion de mémoire : je représente la mémoire avec un `Hashtbl`. Les cas de mémoire sont des `int`s uniques, et cette hashtbl fait un mapping. Les réf se réduisent aux adresses : ces `int`s uniques. Comme dans APS, l'évaluation ici prend un terme et un mémoire.

### Testing

Il existe des tests dans le dossier `test/`. En particulier, ils sont pour les fonctionnalités du typeur et de l'évaluateur, qui prennent les `pterm`s (la représentation AST) en place d'un programme string qui doit être lu par le lexer et le parseur.

### Sources d'inspiration

– Pour les explications et astuces d'implémentation pour le let-polymorphisme et le polymorphisme faible : https://courses.cs.cornell.edu/cs3110/2021sp/textbook/interp/letpoly.html
