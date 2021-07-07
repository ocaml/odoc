`Odoc`'s generators (html, latex, and manpages) are tested by the building on dune's [include](https://dune.readthedocs.io/en/latest/dune-files.html#include) stanza idea to generate a dune file.

### Directory layout
```
generators/
  cases/
    <case_name>.ml
  gen_rules/
    gen_rules.ml
    dune
  html/
    <Case_name>-index.html
    <Case_name>-X-index.html (** X is a module *)
    <case_name>.targets (** contains html targets *)
    ...
  latex/
    ...
  man/
    ...
  dune
  link.dune.inc
  gen_rules_lib.ml
  README.md
  ...
```

`link.dune.inc` is checked out in the repository and is updated by a test rule.

`generators/dune` contains the instruction of loading `link.dune.inc` and defines the rule to
update it.

`gen_rules.ml` is the program to generate `link.dune.inc`.

Dependencies of `link.dune.inc` (information used by `gen_rules.exe` to compute it):
- The list of test cases (this is obtained by scaning the `cases` directory)
- A `.targets` file for each test case, which path is derived from the case's
  name. If this file doesn't exist, we don't fail and assumes an empty list of
  targets.

**Note** :
- when a test added has some new syntax, specifying a minimum bound on the ocaml version is required, and a maximum bound when a bug is fixed. These have to be specified as part of the [constraints](https://github.com/ocaml/odoc/blob/65074eeee3fe8478c4df5099cf83a95326484105/test/generators/gen_rules/gen_rules.ml#L75-L86).

- If a test case is removed, or modified, for example when a module is removed from the test file, the results are expected to reduce but, the artefacts like `.target` file for the corresponding test case and the `html` file (for example) corresponding to the module removed aren't automatically removed from the file system. This thus requires one to manually remove the outdated `html` file.

### To run the tests;
- `dune runtest` -> This generates dune rules to compile, link, generate output targets <`html, latex, manpage`> targets, and the corresponding `.targets` files
- `dune promote` -> promotes the generate rules.
- Second `dune runtest` -> creates the `.target` file corresponding to a certain test case.
- dune promote -> writes to the file system the `.target` file corresponding to a test cases
- Third `dune runtest` -> create the output files ready to be written to file system, for example the html files
- `dune promote` -> writes the output files to the file system.

**Note** : you may need to run `dune clean && dune build` where expected files are not produced.
