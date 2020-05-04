(** The [result] type and a bind operator. This module is meant to be opened. *)
module ResultMonad = struct
  (** Re-export for compat *)
  type ('a, 'b) t = ('a, 'b) Result.result = Ok of 'a | Error of 'b

  let map_error f = function Ok _ as ok -> ok | Error e -> Error (f e)

  let of_option ~error = function Some x -> Ok x | None -> Error error

  let bind m f = match m with Ok x -> f x | Error _ as e -> e

  let (>>=) = bind
end

(** A bind operator for the [option] type. This module is meant to be opened. *)
module OptionMonad = struct
  type 'a t = 'a option = None | Some of 'a

  (* The error case become [None], the error value is ignored. *)
  let of_result = function Result.Ok x -> Some x | Error _ -> None

  let return x = Some x

  let bind m f = match m with Some x -> f x | None -> None

  let (>>=) = bind
end
