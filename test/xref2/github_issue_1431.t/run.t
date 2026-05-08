Repro of #1431. We shouldn't see any errors or warnings from odoc.

  $ ocamlc -bin-annot -c legacy.mli
  $ ocamlc -bin-annot -c top.mli
  $ odoc compile legacy.cmti
  $ odoc compile -I . top.cmti
  odoc: internal error, uncaught exception:
        Failure("error")
        Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
        Called from Odoc_xref2__Tools.apply_inner_substs.inner in file "src/xref2/tools.ml", line 2414, characters 24-34
        Called from Odoc_xref2__Tools.apply_inner_substs in file "src/xref2/tools.ml", line 2417, characters 20-34
        Called from Odoc_xref2__Compile.include_.get_expansion in file "src/xref2/compile.ml", line 430, characters 19-51
        Called from Odoc_xref2__Compile.signature_items.loop in file "src/xref2/compile.ml", line 348, characters 27-41
        Called from Odoc_xref2__Compile.signature in file "src/xref2/compile.ml", line 365, characters 19-49
        Called from Odoc_xref2__Compile.content.(fun) in file "src/xref2/compile.ml", line 100, characters 15-54
        Called from Odoc_xref2__Compile.unit in file "src/xref2/compile.ml", line 68, characters 21-47
        Called from Odoc_xref2__Lookup_failures.with_ref in file "src/xref2/lookup_failures.ml", line 13, characters 10-14
        Called from Odoc_xref2__Lookup_failures.catch_failures in file "src/xref2/lookup_failures.ml", line 60, characters 20-37
        Called from Odoc_odoc__Compile.resolve_and_substitute in file "src/odoc/compile.ml", line 154, characters 4-49
        Called from Odoc_model__Error.catch in file "src/model/error.ml", line 52, characters 21-27
        Called from Odoc_model__Error.catch_warnings.(fun) in file "src/model/error.ml", line 87, characters 18-22
        Called from Odoc_model__Error.with_ref in file "src/model/error.ml", line 65, characters 12-16
        Re-raised at Odoc_model__Error.with_ref in file "src/model/error.ml", line 70, characters 4-11
        Called from Odoc_odoc__Compile.compile.(fun) in file "src/odoc/compile.ml", line 407, characters 6-206
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 22, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 20, characters 12-19
        Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 41, characters 7-16
  [2]
