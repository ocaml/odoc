(** {[
      let x = 1
    ]} *)

val x : int
[@@deriving none]
(** {[
      let () =
        print_int x;
        print_newline ()
    ]} *)

module A : sig
  val x : int
  (** {[
        let hello = 2
      ]} *)
end

type t =
  | A of int
      (** {[
            let _ = hello +. hello
          ]} *)
  | B of int
