Items bound by an extended open (here `open struct ... end`) should not be lost, even
when the block immediately follows the top-of-file documentation comment (see
file.ml). The doc reference {!M.t} below resolves without warning; previously
the open was silently dropped and [M] could not be found.

  $ ocamlc -bin-annot -c file.ml
  $ odoc compile file.cmt

  $ odoc link file.odoc
