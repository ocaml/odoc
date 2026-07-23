let[@zero_alloc] add b x y = if b then x + y else x

module To_be_included = struct
  let[@zero_alloc] add b x y = if b then x + y else x
  (* [add] has a zero alloc annotation that it shouldn't loose *)
end

module Including = struct
  include To_be_included
end

(** {1 Include functor on structures} *)

module No_include_functor = struct
(** This module shows how to achieve the effect without [include functor],
    with an intermediate module [T]. *)
  module Make (T : sig type t end) = struct type included end
  module T = struct
    type t
  end

  include T
  include Make(T)
end

module Include_functor = struct
(** This module demonstratest the [include functor] functionality *)
  module Make (T : sig type t end) = struct type included end
  type t
  include functor Make
end
