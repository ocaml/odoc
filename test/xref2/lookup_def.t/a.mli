module M : sig end

module N : sig
  module type S = sig
    val x : int
  end

  module T : S
end

type t

val a : int

exception Exn

type ext = ..

type ext += Ext

class cls : object end

class type clst = object end
