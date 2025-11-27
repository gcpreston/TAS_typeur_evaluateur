# Typeur-Evaluateur

Le projet typeur-evaluateur pour TAS : Sorbonne Université M2 2025.

## La langue

Ce projet implémente un lexeur, un parseur, un typeur, et un evaluateur pour une langue customisée. Elle suit le plupart des règles de OCaml, et son syntaxe est similaire :

```
let id = fun x -> x in
id 5
```

Plusieurs exemples sont trouvés dans le dossier `examples/`. Il faut noter qu'un programme est une seule expression.

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
