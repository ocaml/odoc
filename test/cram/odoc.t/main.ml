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
