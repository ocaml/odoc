(** The [result] type and a bind operator. This module is meant to be opened. *)
module ResultMonad = struct
  (** Re-export for compat *)
  type ('a, 'b) result = ('a, 'b) Result.result = Ok of 'a | Error of 'b

  let map_error f = function Ok _ as ok -> ok | Error e -> Error (f e)

  let of_option ~error = function Some x -> Ok x | None -> Error error

  let ( >>= ) m f = match m with Ok x -> f x | Error _ as e -> e
end
