open Odoc_model.Lang.Source_code.Info

type local_jmp_infos = jmp_to_def with_pos list

let lines src =
  let lines = String.split_on_char '\n' src in
  let _, poses, _ =
    List.fold_left
      (fun (i, poses, count) line ->
        let l = String.length line in
        let poses = (Line i, (count, count)) :: poses in
        (i + 1, poses, count + l + 1))
      (1, [], 0) lines
  in
  poses

let highlight src =
  Syntax_highlighter.syntax_highlighting_locs src
  |> List.rev_map (fun (x, y) -> (Syntax x, y))
(* The order won't matter and input can be large *)

let of_source ~local_jmp src =
  lines src
  |> List.rev_append (highlight src)
  |> List.rev_append
       (List.rev_map (fun (jmp, pos) -> (Local_jmp jmp, pos)) local_jmp)
