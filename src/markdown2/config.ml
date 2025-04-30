(* Markdown output configuration *)

type t = { root_url : string option }

let v ~root_url () = { root_url }
