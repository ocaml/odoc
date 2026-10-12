open Main__

module Foo = Foo

(* [Zone] is exposed here, but under a different name - so [Main.Zone], the
   canonical path Dune put on the alias in [main__.ml], does not exist. *)
module Private = struct
  module Zone_alias = Zone
end

let zone_name = Zone.name
