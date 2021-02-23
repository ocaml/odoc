module Identifier = struct
  type t = [ `boo ]
end

module Resolved_path = struct
  type module_ = [ `Identifier of Identifier.t | `Hidden of module_ ]
  (** @canonical Foo.Type.Path.t *)
end
