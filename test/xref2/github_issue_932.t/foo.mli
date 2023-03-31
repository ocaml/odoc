(* Consider this extensible type *)

type t = ..

type t +=
| A
| B

(** - {!t}
    - {!extension-decl-A}
    - {!extension-A}
    - {!extension-B} *)
