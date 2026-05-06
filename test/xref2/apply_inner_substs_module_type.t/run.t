Minimal repros of #1429. We shouldn't see any errors from odoc for any of these
examples.

  $ for f in *.mli; do ocamlc -bin-annot -c "$f"; done

Position A — destructive subst at signature level, fragment in a later [include]:

  $ odoc compile a_include.cmti
  odoc: internal error, uncaught exception:
        File "src/xref2/subst.ml", line 456, characters 24-30: Assertion failed
        Raised at Odoc_xref2__Subst.resolved_signature_fragment in file "src/xref2/subst.ml", line 456, characters 24-36
        Called from Odoc_xref2__Subst.resolved_type_fragment in file "src/xref2/subst.ml", line 491, characters 27-58
        Called from Odoc_xref2__Subst.type_fragment in file "src/xref2/subst.ml", line 526, characters 20-48
        Called from Odoc_xref2__Subst.with_module_type_substitution in file "src/xref2/subst.ml", line 741, characters 36-53
        Called from Stdlib__List.map in file "list.ml", line 85, characters 15-19
        Called from Odoc_xref2__Subst.u_module_type_expr in file "src/xref2/subst.ml", line 692, characters 9-56
        Called from Odoc_xref2__Subst.include_decl in file "src/xref2/subst.ml", line 755, characters 31-55
        Called from Odoc_xref2__Subst.include_ in file "src/xref2/subst.ml", line 819, characters 11-32
        Called from Odoc_xref2__Subst.apply_sig_map_item in file "src/xref2/subst.ml", line 1049, characters 25-39
        Called from Stdlib__List.rev_map.rmap_f in file "list.ml", line 107, characters 22-25
        Called from Odoc_xref2__Subst.apply_sig_map_items in file "src/xref2/subst.ml", line 1054, characters 2-43
        Called from Odoc_xref2__Subst.apply_sig_map in file "src/xref2/subst.ml", line 1058, characters 3-30
        Called from Odoc_xref2__Subst.signature in file "src/xref2/subst.ml", line 1011, characters 39-71
        Called from Odoc_xref2__Tools.fragmap.(fun) in file "src/xref2/tools.ml", lines 2016-2022, characters 4-7
        Called from Odoc_xref2__Tools.apply_inner_substs.inner in file "src/xref2/tools.ml", line 2417, characters 14-56
        Called from Odoc_xref2__Tools.apply_inner_substs in file "src/xref2/tools.ml", line 2427, characters 20-34
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
        Called from Odoc_odoc__Compile.compile.(fun) in file "src/odoc/compile.ml", lines 407-410, characters 6-23
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 22, characters 12-19
        Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [2]

Position B — destructive subst at signature level, fragment in a later module type definition:

  $ odoc compile b_modtype_def.cmti
  odoc: internal error, uncaught exception:
        File "src/xref2/subst.ml", line 456, characters 24-30: Assertion failed
        Raised at Odoc_xref2__Subst.resolved_signature_fragment in file "src/xref2/subst.ml", line 456, characters 24-36
        Called from Odoc_xref2__Subst.resolved_type_fragment in file "src/xref2/subst.ml", line 491, characters 27-58
        Called from Odoc_xref2__Subst.type_fragment in file "src/xref2/subst.ml", line 526, characters 20-48
        Called from Odoc_xref2__Subst.with_module_type_substitution in file "src/xref2/subst.ml", line 741, characters 36-53
        Called from Stdlib__List.map in file "list.ml", line 85, characters 15-19
        Called from Odoc_xref2__Subst.module_type_expr in file "src/xref2/subst.ml", line 715, characters 12-70
        Called from Odoc_xref2__Subst.module_type in file "src/xref2/subst.ml", line 652, characters 37-59
        Called from Odoc_xref2__Component.Delayed.get in file "src/xref2/component.ml", line 43, characters 16-22
        Called from Odoc_xref2__Lang_of.module_type in file "src/xref2/lang_of.ml", line 875, characters 12-37
        Called from Odoc_xref2__Lang_of.signature_items.inner in file "src/xref2/lang_of.ml", line 412, characters 48-77
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
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 22, characters 12-19
        Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [2]

Position C — destructive subst at signature level, fragment in a later module declaration:

  $ odoc compile c_module_decl.cmti
  odoc: internal error, uncaught exception:
        File "src/xref2/subst.ml", line 456, characters 24-30: Assertion failed
        Raised at Odoc_xref2__Subst.resolved_signature_fragment in file "src/xref2/subst.ml", line 456, characters 24-36
        Called from Odoc_xref2__Subst.resolved_type_fragment in file "src/xref2/subst.ml", line 491, characters 27-58
        Called from Odoc_xref2__Subst.type_fragment in file "src/xref2/subst.ml", line 526, characters 20-48
        Called from Odoc_xref2__Subst.with_module_type_substitution in file "src/xref2/subst.ml", line 741, characters 36-53
        Called from Stdlib__List.map in file "list.ml", line 85, characters 15-19
        Called from Odoc_xref2__Subst.module_type_expr in file "src/xref2/subst.ml", line 715, characters 12-70
        Called from Odoc_xref2__Subst.module_decl in file "src/xref2/subst.ml", line 750, characters 31-53
        Called from Odoc_xref2__Subst.module_ in file "src/xref2/subst.ml", line 759, characters 14-35
        Called from Odoc_xref2__Component.Delayed.get in file "src/xref2/component.ml", line 43, characters 16-22
        Called from Odoc_xref2__Lang_of.signature_items.inner in file "src/xref2/lang_of.ml", line 407, characters 16-39
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
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 22, characters 12-19
        Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [2]

Position D — destructive subst at signature level, fragment in a functor parameter:

  $ odoc compile d_functor_param.cmti
  odoc: internal error, uncaught exception:
        File "src/xref2/subst.ml", line 456, characters 24-30: Assertion failed
        Raised at Odoc_xref2__Subst.resolved_signature_fragment in file "src/xref2/subst.ml", line 456, characters 24-36
        Called from Odoc_xref2__Subst.resolved_type_fragment in file "src/xref2/subst.ml", line 491, characters 27-58
        Called from Odoc_xref2__Subst.type_fragment in file "src/xref2/subst.ml", line 526, characters 20-48
        Called from Odoc_xref2__Subst.with_module_type_substitution in file "src/xref2/subst.ml", line 741, characters 36-53
        Called from Stdlib__List.map in file "list.ml", line 85, characters 15-19
        Called from Odoc_xref2__Subst.module_type_expr in file "src/xref2/subst.ml", line 715, characters 12-70
        Called from Odoc_xref2__Subst.functor_parameter in file "src/xref2/subst.ml", line 664, characters 41-68
        Called from Odoc_xref2__Subst.module_type_expr in file "src/xref2/subst.ml", line 710, characters 15-38
        Called from Odoc_xref2__Subst.module_decl in file "src/xref2/subst.ml", line 750, characters 31-53
        Called from Odoc_xref2__Subst.module_ in file "src/xref2/subst.ml", line 759, characters 14-35
        Called from Odoc_xref2__Component.Delayed.get in file "src/xref2/component.ml", line 43, characters 16-22
        Called from Odoc_xref2__Lang_of.signature_items.inner in file "src/xref2/lang_of.ml", line 407, characters 16-39
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
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 22, characters 12-19
        Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [2]

Position E — destructive subst at signature level, fragment nested inside a sub-module type:

  $ odoc compile e_nested.cmti
  odoc: internal error, uncaught exception:
        File "src/xref2/subst.ml", line 456, characters 24-30: Assertion failed
        Raised at Odoc_xref2__Subst.resolved_signature_fragment in file "src/xref2/subst.ml", line 456, characters 24-36
        Called from Odoc_xref2__Subst.resolved_type_fragment in file "src/xref2/subst.ml", line 491, characters 27-58
        Called from Odoc_xref2__Subst.type_fragment in file "src/xref2/subst.ml", line 526, characters 20-48
        Called from Odoc_xref2__Subst.with_module_type_substitution in file "src/xref2/subst.ml", line 741, characters 36-53
        Called from Stdlib__List.map in file "list.ml", line 85, characters 15-19
        Called from Odoc_xref2__Subst.module_type_expr in file "src/xref2/subst.ml", line 715, characters 12-70
        Called from Odoc_xref2__Subst.module_decl in file "src/xref2/subst.ml", line 750, characters 31-53
        Called from Odoc_xref2__Subst.module_ in file "src/xref2/subst.ml", line 759, characters 14-35
        Called from Odoc_xref2__Component.Delayed.get in file "src/xref2/component.ml", line 43, characters 16-22
        Called from Odoc_xref2__Lang_of.signature_items.inner in file "src/xref2/lang_of.ml", line 407, characters 16-39
        Called from Odoc_xref2__Lang_of.signature in file "src/xref2/lang_of.ml", line 460, characters 12-43
        Called from Odoc_xref2__Lang_of.module_type_expr in file "src/xref2/lang_of.ml", lines 818-820, characters 8-17
        Called from Odoc_xref2__Component.Opt.map in file "src/xref2/component.ml", line 58, characters 38-43
        Called from Odoc_xref2__Lang_of.module_type in file "src/xref2/lang_of.ml", line 883, characters 11-57
        Called from Odoc_xref2__Lang_of.signature_items.inner in file "src/xref2/lang_of.ml", line 412, characters 48-77
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
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 22, characters 12-19
        Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [2]

Position F — destructive subst inside a [with]-clause, fragment in another [with]-clause of the substituted signature:

  $ odoc compile f_with_modtype_subst.cmti
  odoc: internal error, uncaught exception:
        File "src/xref2/subst.ml", line 456, characters 24-30: Assertion failed
        Raised at Odoc_xref2__Subst.resolved_signature_fragment in file "src/xref2/subst.ml", line 456, characters 24-36
        Called from Odoc_xref2__Subst.resolved_type_fragment in file "src/xref2/subst.ml", line 491, characters 27-58
        Called from Odoc_xref2__Subst.type_fragment in file "src/xref2/subst.ml", line 526, characters 20-48
        Called from Odoc_xref2__Subst.with_module_type_substitution in file "src/xref2/subst.ml", line 741, characters 36-53
        Called from Stdlib__List.map in file "list.ml", line 85, characters 15-19
        Called from Odoc_xref2__Subst.module_type_expr in file "src/xref2/subst.ml", line 715, characters 12-70
        Called from Odoc_xref2__Subst.module_type in file "src/xref2/subst.ml", line 652, characters 37-59
        Called from Odoc_xref2__Component.Delayed.get in file "src/xref2/component.ml", line 43, characters 16-22
        Called from Odoc_xref2__Lang_of.module_type in file "src/xref2/lang_of.ml", line 875, characters 12-37
        Called from Odoc_xref2__Lang_of.signature_items.inner in file "src/xref2/lang_of.ml", line 412, characters 48-77
        Called from Odoc_xref2__Lang_of.signature in file "src/xref2/lang_of.ml", line 460, characters 12-43
        Called from Odoc_xref2__Lang_of.simple_expansion in file "src/xref2/lang_of.ml", line 605, characters 30-51
        Called from Odoc_xref2__Compile.module_type_expr.get_expansion in file "src/xref2/compile.ml", line 705, characters 20-69
        Called from Odoc_xref2__Compile.module_type_expr in file "src/xref2/compile.ml", line 720, characters 24-51
        Called from Odoc_xref2__Compile.module_decl in file "src/xref2/compile.ml", line 382, characters 34-64
        Called from Odoc_xref2__Compile.module_ in file "src/xref2/compile.ml", line 376, characters 19-67
        Called from Odoc_xref2__Compile.signature_items.loop in file "src/xref2/compile.ml", line 277, characters 21-34
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
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
        Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 22, characters 12-19
        Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [2]
