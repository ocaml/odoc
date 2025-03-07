(* Markdown output configuration *)

[@@@warning "-69"]

type t = { root_url : string option }

let v ~root_url () = { root_url }
