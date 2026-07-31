type t = Alpha | Beta

type r = { fld : int }

(** The type is dropped whether or not the reference spelled it out:
    {!Alpha}, {!constructor-Alpha}, {!t.Alpha}, {!t.constructor-Alpha},
    {!fld}, {!field-fld} and {!r.fld}. *)

module Bla : sig
  type ha = Alpha | Beta

  type ra = { fla : int }

  type sw = [ `On | `Off ]

  type ext = ..

  type ext += Ext_a

  exception Exn_a

  module Inner : sig
    type i = Gamma
  end
end

(** The module path is kept: {!Bla.Alpha} and {!Bla.ha.Alpha} both render as
    [Bla.Alpha]; {!Bla.fla} and {!Bla.ra.fla} both render as [Bla.fla].

    Polymorphic constructors lose the type as well: {!Bla.sw.On} and
    {!Bla.sw.`Off}.

    Only the type component is dropped, not the enclosing modules:
    {!Bla.Inner.Gamma} and {!Bla.Inner.i.Gamma}.

    Parents that are not types are left alone: {!Bla.Ext_a}, {!Bla.Exn_a},
    {!Bla.ha} and {!Bla.Inner.i}.

    When the reference carries its own text, the rendered path becomes the
    tooltip instead: {{!Bla.Alpha} the first case}. *)

module Ambiguous : sig
  type a = Same

  type b = Same
end

(** Two types of a same module may share a constructor name; the rendered text
    is then the same for both and only the anchor tells them apart:
    {!Ambiguous.a.Same} and {!Ambiguous.b.Same}. *)

class cls : object end

(** An unresolved reference is still printed as it was written: {!cls.Alpha} *)
