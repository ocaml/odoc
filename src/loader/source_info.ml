open Odoc_model.Lang.Source_info

type local_jmp_infos = jmp_to_def with_pos list

let of_local_jmp local_jmp =
  List.rev_map (fun (jmp, pos) -> (Local_jmp jmp, pos)) local_jmp

let of_source src =
  Syntax_highlighter.syntax_highlighting_locs src
  |> List.rev_map (fun (x, y) -> (Syntax x, y))
(* The order won't matter and input can be large *)
