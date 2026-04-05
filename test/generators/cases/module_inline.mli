(** Tests for the [\@inline] attribute on module declarations.

    When a module declaration carries [\@inline] in its doc comment, its
    contents are rendered directly on the parent page rather than on a separate
    sub-page. This mirrors how [include … (**\@inline*)] works. *)

(** A normal module — contents appear on a separate page (default behaviour). *)
module Normal : sig
  type t
  val create : unit -> t
end

(** An inlined module — contents appear directly on this page.
    @inline *)
module Inlined : sig
  type t
  val create : unit -> t
end

(** A module without an inline signature is unaffected by [\@inline].
    @inline *)
module Alias = Normal

(** Nested: the outer module is inlined; inner sub-modules still get their own
    pages unless they are also marked [\@inline].
    @inline *)
module Outer : sig
  (** Nested module without [\@inline] — separate page. *)
  module Inner : sig
    type u
  end

  val x : int
end
