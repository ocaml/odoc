type t = int
(** A comment *)

(** {1 this is a title}

    and this is a paragraph

    *)

module M = struct
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
