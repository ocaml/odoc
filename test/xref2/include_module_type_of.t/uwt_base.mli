(* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. *)
module Base : sig
  type 'a uv_result = 'a

module Fs_types : sig
  type uv_open_flag =
    | O_RDONLY (** Open for reading *)
  (** Flags for {!Fs_functions.openfile}

      [O_CLOEXEC] doesn't exist, because this flag is unconditionally
      added by libuv. [O_SHARE_DELETE], [O_SHARE_WRITE], [O_SHARE_READ]
      are always added on Windows, unless [O_EXLOCK] is specified. *)

end

module type Fs_functions = sig
  include module type of Fs_types
  with type uv_open_flag = Fs_types.uv_open_flag

  type 'a t

  val openfile : ?perm:int -> mode:uv_open_flag list -> string -> int t
  (** Equivalent to open(2). perm defaults are 0o644 *)
end
end

include module type of Base
  with type Fs_types.uv_open_flag = Base.Fs_types.uv_open_flag

