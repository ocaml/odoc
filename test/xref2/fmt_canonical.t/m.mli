(** @canonical M.Pub *)
module Hidden__pub : sig type t end

module Pub = Hidden__pub

(** @canonical Nonexistent.Other *)
module Hidden__other : sig type t end

module Other = Hidden__other
