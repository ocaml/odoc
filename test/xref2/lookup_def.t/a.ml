module M = struct end

module N = struct
  module type S = sig
    val x : int
  end

  module T : S = struct
    let x = 1
  end
end

type t

let a = 2

(** Not exported *)
let a' = 3

exception Exn

type ext = ..

type ext += Ext

class cls = object end

class type clst = object end
