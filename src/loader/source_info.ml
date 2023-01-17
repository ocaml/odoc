open Odoc_model.Lang.Source_code.Info

type local_jmp_infos = jmp_to_def with_pos list

let lines src =
  let rec loop (i, poses, count) =
    try
      let next_line_index = String.index_from src count '\n' in
      let poses = (Line i, (count, count)) :: poses in
      loop (i + 1, poses, next_line_index + 1)
    with
    | Not_found -> (Line i, (count, count)) :: poses
    | Invalid_argument _ -> poses
  in
  loop (1, [], 0)

let highlight src =
  Syntax_highlighter.syntax_highlighting_locs src
  |> List.rev_map (fun (x, y) -> (Syntax x, y))
(* The order won't matter and input can be large *)

let of_source ~local_jmp src =
  lines src
  |> List.rev_append (highlight src)
  |> List.rev_append
       (List.rev_map (fun (jmp, pos) -> (Local_jmp jmp, pos)) local_jmp)
