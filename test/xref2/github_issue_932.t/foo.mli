(* Consider this extensible type *)

type t = ..

type t +=
| A
| B

module M : sig
  type t = ..

  type t +=
  | A
  | B
end

type M.t += C

(** - t : {!t}
    - extension-decl-A : {!extension-decl-A}
    - extension-decl-B : {!extension-decl-B}
    - extension-A : {!extension-A}
    - extension-B : {!extension-B}
    - A : {!A}

    - M.t : {!M.t}
    - M.extension-decl-A : {!M.extension-decl-A}
    - M.extension-decl-B : {!M.extension-decl-B}
    - M.extension-A : {!M.extension-A}
    - M.extension-B : {!M.extension-B}
    - M.A : {!M.A}
 *)
