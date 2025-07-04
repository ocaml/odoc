type msg = [ `Msg of string ]

(** The [result] type and a bind operator. This module is meant to be opened. *)
module ResultMonad = struct
  let map_error f = function Ok _ as ok -> ok | Error e -> Error (f e)

  let of_option ~error = function Some x -> Ok x | None -> Error error

  let ( >>= ) = Result.bind
end

(** A bind operator for the [option] type. This module is meant to be opened. *)
module OptionMonad = struct
  (* The error case become [None], the error value is ignored. *)
  let of_result = function Ok x -> Some x | Error _ -> None

  let ( >>= ) = Option.bind
end

module List = Odoc_list

module Tree = Tree
module Forest = Tree.Forest
module Json = Json

module Io_utils = struct
  (** [with_open_*] are resource safe wrappers around opening and closing
      channels. They are equivalent to the same functions in OCaml 4.14's
      [In_channel] and [Out_channel]. *)

  let _with_resource res ~close f =
    Fun.protect ~finally:(fun () -> close res) (fun () -> f res)

  let with_open_in fname f =
    _with_resource (open_in fname) ~close:close_in_noerr f

  let with_open_in_bin fname f =
    _with_resource (open_in_bin fname) ~close:close_in_noerr f

  (** Read a file line-by-line by folding [f]. *)
  let fold_lines fname f acc =
    _with_resource (open_in fname) ~close:close_in_noerr (fun ic ->
        let rec loop acc =
          match input_line ic with
          | exception End_of_file -> acc
          | line -> loop (f line acc)
        in
        loop acc)

  (** Read a file as a list of lines. *)
  let read_lines fname =
    List.rev (fold_lines fname (fun line acc -> line :: acc) [])

  let with_open_out fname f =
    _with_resource (open_out fname) ~close:close_out_noerr f

  let with_open_out_bin fname f =
    _with_resource (open_out_bin fname) ~close:close_out_noerr f

  (** Like [with_open_out] but operate on a [Format] buffer. *)
  let with_formatter_out fname f =
    with_open_out fname (fun oc -> f (Format.formatter_of_out_channel oc))

  (** Shortcuts for composing [with_open_*] functions and [Marshal]. *)
  let marshal fname v =
    with_open_out_bin fname (fun oc -> Marshal.to_channel oc v [])

  let unmarshal fname = with_open_in_bin fname Marshal.from_channel
end

module Int = struct
  include Int
  let max x y : t = if x >= y then x else y
end

include Astring
