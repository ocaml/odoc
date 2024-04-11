Compile the files

  $ ocamlc -c main.ml -bin-annot -I .

Compile and link the documentation

  $ odoc compile main.cmt
  $ odoc link main.odoc
  $ odoc compile-index main.odocl

We have a problem: The ID for Y generates an URL to a file which is not
generated (as the module does not have an expansion).

  $ cat index.json | jq | grep url |grep Y
        "url": "Main/Y/index.html",

  $ odoc html-generate -o html main.odocl && ls Main/Y/index.html
  ls: cannot access 'Main/Y/index.html': No such file or directory
  [2]
