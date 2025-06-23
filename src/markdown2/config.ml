(* Markdown output configuration *)

type t = { root_url : string option; allow_html : bool }

let make ~root_url ~allow_html () = { root_url; allow_html }
