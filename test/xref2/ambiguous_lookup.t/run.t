Two directories each have a unit [Foo]. [Main] has a reference to [Foo], and
link is given both directories, so the reference can mean either unit.

  $ (cd a && ocamlc -bin-annot -c foo.mli)
  $ (cd b && ocamlc -bin-annot -c foo.mli)
  $ ocamlc -bin-annot -c main.mli
  $ mkdir -p odoc/a odoc/b
  $ odoc compile --parent-id a --output-dir odoc a/foo.cmti
  $ odoc compile --parent-id b --output-dir odoc b/foo.cmti
  $ odoc compile main.cmti

The warning for the ambiguous lookup:

  $ odoc link -I odoc/a -I odoc/b main.odoc
  File "Foo":
  Ambiguous lookup. Possible files: Foo
  Foo

The warning should be the same whatever the order of the directories:

  $ odoc link -I odoc/b -I odoc/a main.odoc
  File "Foo":
  Ambiguous lookup. Possible files: Foo
  Foo
