# Typeur-Evaluateur

Le projet typeur-evaluateur pour TAS : Sorbonne Université M2 2025.

## La langue

Ce projet implémente un lexeur, un parseur, un typeur, et un evaluateur pour une langue customisée. Elle suit le plupart des règles de OCaml, et son syntaxe est similaire :

```
let id = fun x -> x in
id 5
```

Plusieurs exemples peuvent être trouvés dans le dossier `examples/`. Il faut noter qu'un programme est une seule expression.

Les features implementés sont:
- numéros
- addition, soustraction
- listes
- fonctions, récursion par point fixe
- applications
- if zero, if empty
- let
- ref, deref, assign

## Build et exécution

### Typage

Pour trouver le type d'un programme dans un fichier :

```bash
dune exec type <path/to/program.tas>
# par exemble : dune exec typage examples/factorial.tas
```

### Evaluation

Pour evaluer un programme dans un fichier :

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

De ma connaissance, tout ce qui est décrit dans le sujet marche dans cette implémentation : le lambda-calcul simplement typé, les entiers, les listes, let, let-polymorphisme, ref, deref, l'assignement, et le polymorphisme faible. Par contre, il n'y a pas des éxtensions ; je n'avais pas le temps après régler beacoup de problèmes avec le let-polymorphisme et le polymorphisme faible, qui sont décrits dans la section suivante.

### Défis

#### Let-polymorphisme
Le plus grand défi était avec le let-polymorphisme. Le grand problème était que chaque fois que je pensais que je l'ai fait correctement, la solution à réglé un cas mais a cassé un autre. Par exemple :

```
# programme 1
let id = fun x -> x in
id 5

# programme 2
let id = fun x -> x in
let _ = id 5 in
id []
```

Pour pas mal de temps, ces deux programmes n'était pas correctement typés au même temps. Programme 1 avait soit le bon type `NatType` soit le mauvais type `T`. Également, programme 2 était entre le bon type `[T]` et le mauvais résultat `Pas typable`. Le problème était que en typant `id 5` comme un type général `T`, on permet de typer programme 2, mais si on type `id 5` spécifiquement, on ne le permet pas d'avoir un autre type plus tard. Effectivement, ce que cela veut dire est que le let-polymorphisme ne marche pas.

Le problème enfin était l'instantiation des schemes. Cela était fait dans le génération des équations, mais le résultat de cela est que les nouvelles variables de type ne sont pas liées à l'un l'autre.

La solution est de faire l'instantiation dans l'unification. Comme ça, on conserve les relations entre équations de la génération, en aussi différenciant entre plusieurs usages d'un seule scheme.

#### Polymorphisme faible

Le polymorphisme faible à aussi posé un défi de comprehension. Après avoir compris que l'idée en haut niveau est de simplement parfois éviter d'invoquer la généralisation du let-polymorphisme, il est devenu plus facile à comprendre.

#### Récursion

Le défi finale était la réduction des opérateurs point-fixe. J'ai rencontré des problèmes soit de loop infini quand je définis un point fixe dans un `let`, soit de ne pas évaluer un application au point fixe. Enfin l'erreur était que j'ai fait des réduction sous les lambdas pendant le définition d'une fonction.

### Choix d’implémentation

- Gestion de mémoire : Je représente la mémoire avec un `Hashtbl`. Les cas de mémoire sont des `int`s uniques, et ce hashtbl fait un mapping `int -> pterm`. Les refs se réduisent aux adresses : ces `int`s uniques. Comme dans APS, l'evaluation ici prend un terme et un mémoire

### Testing

Il existe des tests dans le dossier `test/`. En particulier, ils sont pour les fonctionalités du typeur et de l'evaluateur, qui prennent les `pterm`s (la représentation AST) en place d'un programme string qui doit être lu par le lexeur et le parseur.

### Sources d'inspiration

- Pour les explications et astuces d'implémentation pour le let-polymorphisme et le polymorphisme faible : https://courses.cs.cornell.edu/cs3110/2021sp/textbook/interp/letpoly.html
