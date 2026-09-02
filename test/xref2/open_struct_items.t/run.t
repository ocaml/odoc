Items bound by an extended open (here `open struct ... end`) are lost when the block
immediately follows the top-of-file documentation comment (see file.ml), so
the doc reference {!M.t} below fails to resolve:

  $ ocamlc -bin-annot -c file.ml
  $ odoc compile file.cmt

  $ odoc link file.odoc
  File "file.ml", line 10, characters 8-14:
  Warning: Failed to resolve reference unresolvedroot(M).t Couldn't find "M"
