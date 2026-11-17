Library [w] has a module [Lib] with a submodule [X], and library [y] has a
different [Lib] with an [X]. Library [a] is built against [w], and its module
type [S] has a module [X] whose canonical path is [Lib.X]. Library [b] is
built against [a], and so against [w] too. Its units [Early] and [Late] each
declare [M : A.S] and refer to [M.X.t]. No library is built against [y], so
no unit sees both [Lib]s (D3, section 2). [y] is only given at link. Link
often needs more libraries than a unit was built against, for example so
that the docs can refer to a library the code does not use.

  $ cat w/lib.mli
  module X : sig
    type t
  end
  $ cat y/lib.mli
  module X : sig
    type t
  end
  $ cat a/a.mli
  module type S = sig
    module X : sig
      type t
    end
    (** @canonical Lib.X *)
  end
  $ cat b/early.mli
  module M : A.S
  
  type t = M.X.t
  $ cat b/late.mli
  module M : A.S
  
  type t = M.X.t
  $ (cd w && ocamlc -bin-annot -c lib.mli)
  $ (cd y && ocamlc -bin-annot -c lib.mli)
  $ (cd a && ocamlc -bin-annot -c a.mli)
  $ (cd b && ocamlc -bin-annot -I ../a -c early.mli late.mli)
  $ odoc compile --output-dir h --parent-id w -L w:h/w w/lib.cmti
  $ odoc compile --output-dir h --parent-id y -L y:h/y y/lib.cmti

[Early] is compiled before [A], which it uses. That breaks D5 (section
2) on purpose; dune and odoc_driver never do it. [A.S] is then left for
link to expand. [Late] is compiled after [A], so compile copies the
expansion of [A.S], with the canonical path, into [Late]. D5 only matters
at compile, so the links of [Early] and [Late] should give the same
result.

  $ odoc compile --output-dir h --parent-id b -L b:h/b -L a:h/a -L w:h/w b/early.cmti
  $ odoc compile --output-dir h --parent-id a -L a:h/a -L w:h/w a/a.cmti
  $ odoc compile --output-dir h --parent-id b -L b:h/b -L a:h/a -L w:h/w b/late.cmti
  $ canonical() { odoc_print $1 | jq -c '[.. | objects | .["`Canonical"]? | select(. != null) | .[1] | .. | objects | .["`Root"]? | select(type == "array") | select(.[0] | type == "object") | .[0].Some["`Page"][1] + "/" + .[1]] | unique'; }

[canonical FILE] lists the roots that the canonical paths in FILE resolved
to, as [parent/Unit], where [parent] is the last part of the unit's
[--parent-id], here its library. It reads odoc's JSON with [jq].

The author of [a] means [w]'s [Lib]. But a canonical tag is a reference, and
its root is looked up in every library given at link (S1 and R3, sections 5
and 4.2). Both [w] and [y] have a [Lib], so the tag is ambiguous. odoc should
warn, and leave the tag unresolved, so [M.X] is shown as it is (S2). In
[Early], [M.X] comes from [A] at link time.

A resolved tag, to either library's [Lib], would be wrong. odoc warns and
takes one (section 11, "Ambiguous names").

  $ odoc link h/b/early.odoc -L b:h/b -L a:h/a -L w:h/w -L y:h/y
  File "Lib":
  Ambiguous lookup. Possible units: w/Lib, y/Lib
  $ canonical h/b/early.odocl
  ["y/Lib"]

In [Late], the canonical path is part of [Late]'s own content, copied by
compile. It is ambiguous in the same way, and the tag should stay unresolved.

Here too, a resolved tag would be wrong (section 11, "Ambiguous names").

  $ odoc link h/b/late.odoc -L b:h/b -L a:h/a -L w:h/w -L y:h/y
  File "Lib":
  Ambiguous lookup. Possible units: w/Lib, y/Lib
  $ canonical h/b/late.odocl
  ["y/Lib"]
