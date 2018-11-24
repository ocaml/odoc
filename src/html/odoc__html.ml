module Html_tree = Html_tree
module Comment = Comment
module To_html_tree = struct
  module ML = To_ml_html_tree
  module RE = To_re_html_tree
end
module List_targets = List_targets

(* Exposed as an unstable public API for third-party packages to "hack" on, see

    https://github.com/ocaml/odoc/pull/252
    https://github.com/ocaml/odoc/issues/236. *)
module Url = Url
