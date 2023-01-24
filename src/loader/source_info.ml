open Odoc_model.Lang.Source_code.Info

type local_jmp_infos = jmp_to_def with_pos list

let highlight src =
  Syntax_highlighter.syntax_highlighting_locs src
  |> List.rev_map (fun (x, y) -> (Syntax x, y))
(* The order won't matter and input can be large *)

let of_source ~local_jmp src =
  highlight src
  |> List.rev_append
       (List.rev_map (fun (jmp, pos) -> (Local_jmp jmp, pos)) local_jmp)
