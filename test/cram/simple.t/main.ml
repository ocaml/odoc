type t = int
(** A comment *)

(** {1 this is a title}

    and this is a paragraph

    *)

module type Signature = sig end

class istack =
  object
    val mutable v = [ 0; 2 ]

    method pop =
      match v with
      | hd :: tl ->
          v <- tl ;
          Some hd
      | [] -> None

    method push hd = v <- hd :: v
  end

class type my_class_type = object end

module Modulule = struct
  type t
  (** dsdsd *)
end

(**  a reference {!t}, and some {e formatted} {b content} with [code] and

{[
  code blocks
]}

 *)
let v = 9

(** lorem 1
  *)
let lorem _ = 'a'

(** lorem 2
  *)
let lorem2 _ = 'a'

(** lorem 3
  *)
let lorem3 _ = 'e'

(** lorem 4
  *)
module Trucmuche = struct
  let bidule = 4
end

include Trucmuche

let lorem4 = 1

type my_type = int * char

type babar =
  | A of string
  | B
  | C of
      { z : int
      ; w : char
      }

type _ celeste =
  { x : babar
  ; y : int -> string
  }

type 'a list =
  | Cons of 'a * 'a list
  | Nil

(** Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
    tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
    quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo 
    consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
    cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
    non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. *)
let long = 3

type ext_t = ..
type ext_t += Ext_const of int
