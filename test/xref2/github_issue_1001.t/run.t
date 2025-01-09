  $ ocamlc -c -bin-annot test.ml
  $ odoc compile test.cmt
  odoc: internal error, uncaught exception:
        File "src/loader/cmi.ml", line 459, characters 21-27: Assertion failed
        Raised at Odoc_loader__Cmi.read_type_expr in file "src/loader/cmi.ml", line 459, characters 21-33
        Called from Odoc_loader__Cmt.read_pattern in file "src/loader/cmt.ml", line 50, characters 22-57
        Called from Odoc_loader__Cmt.read_value_bindings.(fun) in file "src/loader/cmt.ml", line 106, characters 18-50
        Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
        Called from Odoc_loader__Cmt.read_value_bindings in file "src/loader/cmt.ml", line 100, characters 4-401
        Called from Odoc_loader__Cmt.read_structure.(fun) in file "src/loader/cmt.ml", line 609, characters 24-61
        Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
        Called from Odoc_loader__Cmt.read_structure in file "src/loader/cmt.ml", line 607, characters 4-127
        Called from Odoc_loader__Cmt.read_implementation in file "src/loader/cmt.ml", line 624, characters 4-124
        Called from Odoc_loader.read_cmt in file "src/loader/odoc_loader.ml", line 181, characters 12-71
        Called from Odoc_loader.wrap_errors.(fun) in file "src/loader/odoc_loader.ml", line 248, characters 10-14
        Called from Odoc_model__Error.catch in file "src/model/error.ml", line 54, characters 21-27
        Called from Odoc_model__Error.catch_warnings.(fun) in file "src/model/error.ml", line 89, characters 18-22
        Called from Odoc_model__Error.with_ref in file "src/model/error.ml", line 67, characters 12-16
        Re-raised at Odoc_model__Error.with_ref in file "src/model/error.ml", line 72, characters 4-11
        Called from Odoc_odoc__Compile.resolve_and_substitute in file "src/odoc/compile.ml", line 133, characters 8-76
        Called from Odoc_model__Error.catch in file "src/model/error.ml", line 54, characters 21-27
        Called from Odoc_model__Error.catch_warnings.(fun) in file "src/model/error.ml", line 89, characters 18-22
        Called from Odoc_model__Error.with_ref in file "src/model/error.ml", line 67, characters 12-16
        Re-raised at Odoc_model__Error.with_ref in file "src/model/error.ml", line 72, characters 4-11
        Called from Odoc_odoc__Compile.compile.(fun) in file "src/odoc/compile.ml", line 354, characters 6-216
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 22, characters 12-19
        Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [2]

