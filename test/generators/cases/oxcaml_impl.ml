let[@zero_alloc] add b x y = if b then x + y else x

module To_be_included = struct
  let[@zero_alloc] add b x y = if b then x + y else x
  (* [add] has a zero alloc annotation that it shouldn't loose *)
end

module Including = struct
  include To_be_included
end

(* A kind abbreviation defined in an implementation. *)
kind_ my_abbrev = value_or_null mod non_null global

(* A type that uses the abbreviation; the use should link to the definition. *)
type t_abbrev : my_abbrev mod immutable

(* Shadowing: [dup] brought in by the include is shadowed by the local [dup].
   Only the local definition should be rendered (no duplicate anchor). *)
module Shadowing_source = struct
  kind_ dup = value mod portable
end

include Shadowing_source

kind_ dup = value_or_null mod non_null
