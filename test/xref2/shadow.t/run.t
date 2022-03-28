This test contains a carefully crafted couple of modules that cause a clash of names in the current shadowing code.
If the shadowing code is working correctly this should not crash

  $ ocamlc -c -bin-annot -I . c.mli
  $ ocamlc -c -bin-annot -I . d.mli
  $ odoc compile -I . c.cmti
  $ odoc compile -I . d.cmti
  odoc: internal error, uncaught exception:
        Not_found
        Raised at Stdlib__Map.Make.find in file "map.ml", line 137, characters 10-25
        Called from Odoc_xref2__Env.ElementsByName.remove in file "src/xref2/env.ml", line 122, characters 12-33
        Called from Odoc_xref2__Env.remove in file "src/xref2/env.ml", line 247, characters 11-52
        Called from Odoc_xref2__Env.update_module in file "src/xref2/env.ml", line 311, characters 2-23
        Called from Odoc_xref2__Compile.signature_items.(fun) in file "src/xref2/compile.ml", line 214, characters 16-122
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Compile.signature_items in file "src/xref2/compile.ml", line 200, characters 4-1023
        Called from Odoc_xref2__Compile.signature in file "src/xref2/compile.ml", line 269, characters 18-48
        Called from Odoc_xref2__Compile.include_.get_expansion in file "src/xref2/compile.ml", line 346, characters 10-45
        Called from Odoc_xref2__Compile.signature_items.(fun) in file "src/xref2/compile.ml", line 242, characters 21-35
        Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Compile.signature_items in file "src/xref2/compile.ml", line 200, characters 4-1023
        Called from Odoc_xref2__Compile.signature in file "src/xref2/compile.ml", line 269, characters 18-48
        Called from Odoc_xref2__Compile.content.(fun) in file "src/xref2/compile.ml", line 65, characters 15-54
        Called from Odoc_xref2__Compile.unit in file "src/xref2/compile.ml", line 58, characters 21-47
        Called from Odoc_xref2__Lookup_failures.with_ref in file "src/xref2/lookup_failures.ml", line 13, characters 10-14
        Called from Odoc_xref2__Lookup_failures.catch_failures in file "src/xref2/lookup_failures.ml", line 60, characters 20-37
        Called from Odoc_odoc__Compile.resolve_and_substitute in file "src/odoc/compile.ml", line 93, characters 4-49
        Called from Odoc_model__Error.catch in file "src/model/error.ml", line 52, characters 21-27
        Called from Odoc_model__Error.catch_warnings.(fun) in file "src/model/error.ml", line 87, characters 18-22
        Called from Odoc_model__Error.with_ref in file "src/model/error.ml", line 65, characters 12-16
        Re-raised at Odoc_model__Error.with_ref in file "src/model/error.ml", line 70, characters 4-11
        Called from Odoc_odoc__Compile.compile.(fun) in file "src/odoc/compile.ml", line 226, characters 6-136
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 25, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 23, characters 12-19
        Called from Cmdliner.Term.run in file "cmdliner.ml", line 117, characters 32-39
  [2]

