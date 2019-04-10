module type S = sig
  type 'a t
  val init : string t
end

module M : functor (T:sig type 'a t end) ->S with type 'b t := 'b T.t

module type Z = sig
  type t = ..
  type t += Foo
end

type z = ..

module N : Z with type t := z

module L : sig
  type 'a t = 'a list
  val empty : 'a t
end

module type R = sig
  module T : sig
    type 'a t
    val empty : 'a t
  end

  val z : int T.t
end

module P : R with module T := L

(*
module Map : sig
  module Make : functor (T:sig type t end) -> sig
    type 'a t
    val empty : 'a t
  end
end
   *)

module C : sig
  include R with module T := Map.Make (String)
end

module type X = sig
    include S
    val doit : 'a t -> unit
end

(* This case doesn't work properly currently. In the end result the type 'a t
 * is substituted for doit but not for mentions from inside S and the
 * definition of the type is still there. It appears that the type is actually
 * substituted away at first but later expand_include comes along and replaces
 * the include expansion with one that hasn't been rewritten. *)
module type X2 = X with type 'a t := 'a list
