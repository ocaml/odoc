A quick test to repro the issue found in #587

  $ ./build.sh
  odoc: internal error, uncaught exception:
        Failure("Error")
        Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
        Called from Odoc_xref2__Subst.rename_bound_idents.new_module_id in file "src/xref2/subst.ml", line 771, characters 13-29
        Called from Odoc_xref2__Subst.rename_bound_idents.(fun) in file "src/xref2/subst.ml", line 809, characters 16-32
        Called from Odoc_xref2__Subst.rename_bound_idents.(fun) in file "src/xref2/subst.ml", line 868, characters 8-69
        Called from Odoc_xref2__Subst.signature in file "src/xref2/subst.ml", line 907, characters 17-50
        Called from Odoc_xref2__Subst.simple_expansion in file "src/xref2/subst.ml", line 503, characters 30-46
        Called from Odoc_xref2__Subst.option_ in file "src/xref2/subst.ml", line 412, characters 51-61
        Called from Odoc_xref2__Subst.module_type_expr in file "src/xref2/subst.ml", line 599, characters 24-62
        Called from Odoc_xref2__Subst.module_type in file "src/xref2/subst.ml", line 509, characters 37-59
        Called from Odoc_xref2__Tools.handle_module_type_lookup.(fun) in file "src/xref2/tools.ml", line 415, characters 13-37
        Called from Odoc_xref2__Tools.resolve_module_type.(fun) in file "src/xref2/tools.ml", line 708, characters 8-68
        Called from Odoc_xref2__Expand_tools.aux_expansion_of_u_module_type_expr in file "src/xref2/expand_tools.ml", line 111, characters 6-61
        Called from Odoc_xref2__Compile.include_.get_expansion in file "src/xref2/compile.ml", line 325, characters 10-66
        Called from Odoc_xref2__Compile.signature_items.(fun) in file "src/xref2/compile.ml", line 240, characters 21-35
        Called from Stdlib__list.fold_left in file "list.ml", line 121, characters 24-34
        Called from Odoc_xref2__Compile.signature_items in file "src/xref2/compile.ml", line 204, characters 4-1023
        Called from Odoc_xref2__Compile.signature in file "src/xref2/compile.ml", line 273, characters 16-46
        Called from Odoc_xref2__Compile.content.(fun) in file "src/xref2/compile.ml", line 66, characters 13-54
        Called from Odoc_xref2__Compile.unit in file "src/xref2/compile.ml", line 59, characters 21-47
        Called from Odoc_xref2__Lookup_failures.catch_failures in file "src/xref2/lookup_failures.ml", line 18, characters 10-14
        Called from Odoc_odoc__Compile.resolve_and_substitute.(fun) in file "src/odoc/compile.ml", line 74, characters 2-37
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 25, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 23, characters 12-19
        Called from Cmdliner.Term.run in file "cmdliner.ml", line 117, characters 32-39
  [2]


