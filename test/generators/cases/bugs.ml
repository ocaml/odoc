type 'a opt = 'a option
let foo (type a) ?(bar : a opt) () = ()
(** Triggers an assertion failure when
    {:https://github.com/ocaml/odoc/issues/101} is not fixed. *)

let repeat x y = (x, y, x, y)
(** Renders as [val repeat : 'a -> 'b -> 'c * 'd * 'e * 'f] before https://github.com/ocaml/odoc/pull/1173 *)
