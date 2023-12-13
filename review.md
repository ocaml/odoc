# review sherlodoc

# To discuss

- Tester de virer la compression?

- Type extensions: we might want to search for all extensions of a given extensible type.

## With Arthur

- pretty-query: vraiment necessaire ?

- ask about `Query.paths_arrow` vs `index/load_doc.type_distance_paths`

# done

- Réfléchir à `ancient`: enlever pour de bon, rétablir le support ?
Support is reestablished

- `sherlodoc_index` `--db` pourrait avoir pour alias `-o` done

- piper les test avec find dans sort: more robust tests?

- About search-uri: either remove or precise comment (see "If they are relative,
they are interpreted as relative to the `-o` option")

- gérer les typedecl_param dans la CLI?

- Factor or reuse kind to string function in cli to use odoc conversion. Put it in `elt.ml`?
  > No because db does not have a dep on odoc, and it arguably does not need one

- Make it one single type in `succ.mli` (builder vs t)

- Option prendre un fichier contenant la liste des `odocl` ?

- `index` supprimer les `.db`

- `succ.ml` : remove `All`

- Documenter parser/lexer de query.

# Commentaires/Action Point/...

- refactor `Query.paths_arrow` vs `index/load_doc.type_distance_paths`
  `Query.paths_arrow` is the right implementation, load_doc should tranform the
  odoc typeexpr into a sherlodoc query ast typeexpr and then only compute the
  path.
  Be careful about hash consing.

- Have something more robust than sizes in tests. Remove them, and use
  current-bench or just a manual benchmark.

- la limitation sur le packages de query n'est plus vraiment fonctionelle


- `Index.Load_doc.with_tokenizer`: think of which character form a word

- It would be cool to be able to see the string corresponding to types, and also of the intermediate string list list

- Maybe store all "arbitrary constants" relative to the cost function somewhere

<!-- - Essayer de comprendre dans `Load_doc.type_path` pourquoi l'ordre ne fait pas d'importance (see) -->


- `succ.ml` : soit catcher uniquement StackOverflow, soit catcher tout mais moins profondément !? Dans le jsoo sans doute.

- `Succ.All` is used in `query.ml`

# Explications commentée

## Index

- shard est la liste des parties éclatée de la bdd, pour des raisons de mémoire
  plus que de performcances. Maintenant il n'y en a toujours plus qu'un mais ça
  a été gardé. Functionel, mais index n'est plus capable de générer des shards...
- `With_elts` -> pour la partie type-agnostic de la recherche
- `With_occ` -> pour la partie purely type-dependent de la recherche

## Indexation

## Hierarchy structure

Folders:

- `db/` is for the db datastructure. Two datastructures: one for the type
  agnostic part (`db_names`, or `with_elts`) and one for the type-centric part
  of the query (`db_types`, or `with_occs`)
- `index/` is for the action of indexing. Includes a binary.
- `jsoo` the js access to perform query. Compile to a js file to run on a webworker. Read the (marhsalled,
  compressed) db from a global variable: `sherlodoc_db`.
- `cli/` the `cli` access to perform queries. Load the db from a file.
- `store/` is the access to the database. The two directories above (`jsoo` and
  `cli`) use the `storage_js` and `storage_marshal` modules for their purpose.
- `www/` for the webserver running on <https://doc.sherlocode.com>.
- `static/` static files also for the webserver
- `test/` self-explained
- `query/` defines queries and perform them

# Notes personnelles/explications/...

## Index



### Indexation

Pour les textes, c'est facile :
- On crée le payload à partir de la search entry
- On ajoute ça au writer

Pour les types, ça marche pareil mais on doit transformer le type en une string.
Cela est fait par les fonctions suivantes:
- `Load_doc.type_paths` qui prend
  - en entrée le type vu par odoc, un prefix (?) et un signe
  - en sortie, une string list list. Un élément de la liste est une "feuille", l'ordre n'ayant pas d'importance.
- `Db.store_type_path` qui transforme la `string list list` en "concaténant les
  path regroupés !" (qui compte les occurrences de chaque type)







