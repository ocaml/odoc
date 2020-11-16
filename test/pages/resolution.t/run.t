Check resolution works

  $ ocamlc -c -bin-annot m1.mli
  $ odoc compile top1.mld --child sub1
  $ odoc compile sub1.mld -I . --parent top1
  $ odoc link -I . page-top1.odoc
  odoc: internal error, uncaught exception:
        Failure("Child unimplemented")
        Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
        Called from Odoc_xref2__Link.comment_inline_element in file "src/xref2/link.ml", line 144, characters 12-45
        Called from Odoc_xref2__Lookup_failures.with_location in file "src/xref2/lookup_failures.ml", line 36, characters 10-14
        Called from Odoc_xref2__Link.with_location in file "src/xref2/link.ml", line 201, characters 4-71
        Called from Stdlib__list.map in file "list.ml", line 92, characters 20-23
        Called from Odoc_xref2__Link.comment_nestable_block_element in file "src/xref2/link.ml", line 167, characters 17-75
        Called from Odoc_xref2__Lookup_failures.with_location in file "src/xref2/lookup_failures.ml", line 36, characters 10-14
        Called from Odoc_xref2__Link.with_location in file "src/xref2/link.ml", line 201, characters 4-71
        Called from Stdlib__list.map in file "list.ml", line 92, characters 20-23
        Called from Stdlib__list.map in file "list.ml", line 92, characters 32-39
        Called from Stdlib__list.map in file "list.ml", line 92, characters 32-39
        Called from Odoc_xref2__Link.resolve_page.(fun) in file "src/xref2/link.ml", line 823, characters 10-75
        Called from Odoc_xref2__Lookup_failures.catch_failures in file "src/xref2/lookup_failures.ml", line 18, characters 10-14
        Called from Odoc_odoc__Odoc_link.from_odoc.(fun) in file "src/odoc/odoc_link.ml", line 10, characters 4-49
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 25, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 23, characters 12-19
        Called from Cmdliner.Term.run in file "cmdliner.ml", line 117, characters 32-39
  [2]
  $ odoc link -I . page-sub1.odoc
