type t = int
(** A comment *)

module X = struct
  (** A value inside a module  *)
  let c = 1
end

(** A comment aaaaaaaaaa *)
type tdzdz = A of int * int | B of int list * int  (** Bliiiiiiiiiii *)

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

(** lorem 1 and a {{:http://perdu.com}link}
  *)
let lorem = 1

(** lorem 2
  *)
let lorem2 = 1

(** lorem 3
  *)
let lorem3 = 1

(** lorem 4
  *)
let lorem4 = 1

module I = struct
  let x = 1

  (** a paragraph

      and another

      {v verbatim v}

      {m x + 1}

      {[blibli]}
       *)

  let y = 1
end

include I

include J
