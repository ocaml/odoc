[odoc_print --refs] prints the references in the doc comments of a unit or a
page, one per line. A resolved reference is printed as [r(...)].

  $ cat a.mli b.mli index.mld
  (** [A] refers to {!B.t}, and to {!Missing}, which does not exist. *)
  
  type t
  
  (** {!modules: B} *)
  
  module M : sig
    (** Inside [M], {!t} is [A]'s [t]. *)
  
    val v : t
    (** @raise Not_found when {!B.t} is empty. *)
  end
  (** [B] uses {!A.t}. *)
  
  type t
  {0 Index}
  
  See {!page-other} and {!A.M}. {!/pkg/nope} does not exist.
  $ ocamlc -bin-annot -c b.mli a.mli
  $ for m in b a; do odoc compile --output-dir h --parent-id pkg -I h/pkg $m.cmti; done
  $ for f in index other; do odoc compile --output-dir h --parent-id pkg $f.mld; done
  $ odoc link -I h/pkg h/pkg/b.odoc
  $ odoc link -I h/pkg h/pkg/a.odoc
  File "a.mli", line 1, characters 33-43:
  Warning: Failed to resolve reference unresolvedroot(Missing) Couldn't find "Missing"
  File "a.mli", line 11, characters 6-44:
  Warning: Failed to resolve reference unresolvedroot(Not_found) Couldn't find "Not_found"
  $ odoc link -I h/pkg h/pkg/page-index.odoc
  File "index.mld", line 3, characters 30-42:
  Warning: Failed to resolve reference /pkg/nope Path '/pkg/nope' not found

The references of [A] come from its top comment, from nested signatures and
from tags. A [{!modules:}] entry is printed with [modules:], and the references
in its synopsis are indented below it. The [@raise] tag refers to [Not_found].
[Missing] and [Not_found] do not resolve.

  $ odoc_print --refs h/pkg/a.odocl
  r(B.t)
  unresolvedroot(Missing)
  modules: r(B)
    r(A.t)
  r(A.t)
  unresolvedroot(Not_found)
  r(B.t)

Each reference in [where.mli] names the place it is written in. [Here] does
not exist, so none of them resolve, and the link warnings are not shown.

  $ cat where.mli
  (** {!Here.top_comment}
  
      {1 Heading {!Here.heading}}
  
      {{!Here.link_text}text}
  
      {!modules: Here.module_list}
  
      {table {tr {td {!Here.table_cell}}}}
  
      @see <x> {!Here.see_tag}
      @raise Not_found {!Here.raise_tag} *)
  
  type variant =
    | Constructor  (** {!Here.constructor} *)
    | Inline of { field : int  (** {!Here.inline_field} *) }
  
  type record = { field : int  (** {!Here.record_field} *) }
  
  type poly = [ `A  (** {!Here.poly_constructor} *) ]
  
  type ext = ..
  
  type ext += Ext  (** {!Here.extension_constructor} *)
  
  exception Exn of { arg : int  (** {!Here.exception_field} *) }
  
  module type S = sig
    val in_module_type : int  (** {!Here.module_type} *)
  end
  
  module F (X : sig
    val param : int  (** {!Here.functor_parameter} *)
  end) : sig
    val body : int  (** {!Here.functor_body} *)
  end
  
  class c : object
    method m : int  (** {!Here.method_} *)
  
    val v : int  (** {!Here.instance_variable} *)
  end
  
  class type ct = object
    method n : int  (** {!Here.class_type_method} *)
  end
  
  include sig
    val included : int  (** {!Here.include_of_signature} *)
  end
  
  include sig
    type t
  
    val with_ : t  (** {!Here.include_of_signature_with} *)
  end
  with type t := int
  
  module type MS := sig
    val in_subst : int  (** {!Here.module_type_substitution} *)
  end
  
  (** {!Here.include_doc} *)
  include S
  
  (**/**)
  
  val hidden : int  (** {!Here.after_stop} *)
  $ ocamlc -bin-annot -c where.mli
  $ odoc compile --output-dir h --parent-id pkg -I h/pkg where.cmti
  $ odoc link -I h/pkg h/pkg/where.odoc 2>/dev/null

The references of [S] are printed once. [include S] takes its items from [S],
so its expansion is not followed. The references in the two
[include sig ... end] are printed, because those signatures are written here.
The reference after the [(**/**)] stop comment is printed too, although odoc
does not render it.

  $ odoc_print --refs h/pkg/where.odocl
  unresolvedroot(Here).top_comment
  unresolvedroot(Here).heading
  unresolvedroot(Here).link_text
  modules: unresolvedroot(Here).module_list
  unresolvedroot(Here).table_cell
  unresolvedroot(Here).see_tag
  unresolvedroot(Not_found)
  unresolvedroot(Here).raise_tag
  unresolvedroot(Here).constructor
  unresolvedroot(Here).inline_field
  unresolvedroot(Here).record_field
  unresolvedroot(Here).poly_constructor
  unresolvedroot(Here).extension_constructor
  unresolvedroot(Here).exception_field
  unresolvedroot(Here).module_type
  unresolvedroot(Here).functor_parameter
  unresolvedroot(Here).functor_body
  unresolvedroot(Here).method_
  unresolvedroot(Here).instance_variable
  unresolvedroot(Here).class_type_method
  unresolvedroot(Here).include_of_signature
  unresolvedroot(Here).include_of_signature_with
  unresolvedroot(Here).module_type_substitution
  unresolvedroot(Here).include_doc
  unresolvedroot(Here).after_stop

[C.X] is an alias to the hidden [C__x], canonically named [C.X]. Short paths
show the alias. Only long paths show its hidden and canonical parts.

  $ printf '(** @canonical C.X *)\n\ntype t\n' > c__x.mli
  $ printf 'module X = C__x\n\n(** {!X.t} *)\nval v : X.t\n' > c.mli
  $ ocamlc -bin-annot -c c__x.mli c.mli
  $ for m in c__x c; do odoc compile --output-dir h --parent-id pkg -I h/pkg $m.cmti; done
  $ odoc link -I h/pkg h/pkg/c.odoc
  $ odoc_print --refs h/pkg/c.odocl
  r(alias(C.X,C.X).t)
  $ odoc_print --refs --long-paths h/pkg/c.odocl
  r(alias(canonical(hidden(root(C__x)),resolved(root(C).X)),root(C).X).t)

[--refs] prints a whole unit or page. Only [--long-paths] and
[--show-root-parents] can be used with it.

  $ odoc_print --refs -r v h/pkg/c.odocl
  Error: --refs can only be used with --long-paths and --show-root-parents
  [1]
  $ odoc_print --refs --short h/pkg/c.odocl
  Error: --refs can only be used with --long-paths and --show-root-parents
  [1]

A page has references too. An unresolved path reference is printed as written.

  $ odoc_print --refs h/pkg/page-index.odocl
  r(other)
  r(A.M)
  /pkg/nope

With [--show-root-parents], roots are printed with their parent pages.

  $ odoc_print --refs --show-root-parents h/pkg/a.odocl
  r(pkg/B.t)
  unresolvedroot(Missing)
  modules: r(pkg/B)
    r(pkg/A.t)
  r(pkg/A.t)
  unresolvedroot(Not_found)
  r(pkg/B.t)

