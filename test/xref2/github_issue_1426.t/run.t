  $ ocamlc -bin-annot -c test.mli
  $ odoc compile test.cmti
  Exception Not_found handling type_expr: ((module_arg)) -> resolved((param root(module_arg_1_) M).t)
  backtrace:
  Raised at Stdlib__List.assoc in file "list.ml", line 201, characters 10-25
  Called from Odoc_xref2__Lang_of.type_expr_module_arg in file "src/xref2/lang_of.ml", line 1022, characters 11-48
  Called from Odoc_xref2__Lang_of.type_expr in file "src/xref2/lang_of.ml", line 1053, characters 16-53
  
  odoc: internal error, uncaught exception:
        Not_found
        Raised at Stdlib__List.assoc in file "list.ml", line 201, characters 10-25
        Called from Odoc_xref2__Lang_of.type_expr_module_arg in file "src/xref2/lang_of.ml", line 1022, characters 11-48
        Called from Odoc_xref2__Lang_of.type_expr in file "src/xref2/lang_of.ml", line 1053, characters 16-53
        Re-raised at Odoc_xref2__Lang_of.type_expr in file "src/xref2/lang_of.ml", line 1061, characters 4-11
        Called from Odoc_xref2__Lang_of.value_ in file "src/xref2/lang_of.ml", line 693, characters 12-70
        Called from Odoc_xref2__Lang_of.signature_items.inner in file "src/xref2/lang_of.ml", line 432, characters 26-50
        Called from Odoc_xref2__Lang_of.signature in file "src/xref2/lang_of.ml", line 460, characters 12-43
        Called from Odoc_xref2__Lang_of.simple_expansion in file "src/xref2/lang_of.ml", line 605, characters 30-51
        Called from Odoc_xref2__Compile.include_.get_expansion in file "src/xref2/compile.ml", line 431, characters 25-71
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
        Called from Odoc_odoc__Compile.compile.(fun) in file "src/odoc/compile.ml", lines 407-410, characters 6-23
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 22, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 20, characters 12-19
        Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 41, characters 7-16
  [2]
