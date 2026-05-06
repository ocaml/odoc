Minimal repros of #1429. We shouldn't see any errors from odoc for any of these
examples.

  $ for f in *.mli; do ocamlc -bin-annot -c "$f"; done

Position A — destructive subst at signature level, fragment in a later [include]:

  $ odoc compile a_include.cmti

Position B — destructive subst at signature level, fragment in a later module type definition:

  $ odoc compile b_modtype_def.cmti

Position C — destructive subst at signature level, fragment in a later module declaration:

  $ odoc compile c_module_decl.cmti

Position D — destructive subst at signature level, fragment in a functor parameter:

  $ odoc compile d_functor_param.cmti

Position E — destructive subst at signature level, fragment nested inside a sub-module type:

  $ odoc compile e_nested.cmti

Position F — destructive subst inside a [with]-clause, fragment in another [with]-clause of the substituted signature:

  $ odoc compile f_with_modtype_subst.cmti
