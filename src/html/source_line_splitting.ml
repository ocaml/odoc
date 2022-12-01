type line = int

let tag_of_line line = line

let lines_locs src =
  let lines = String.split_on_char '\n' src in
  let _, poses, _ =
    List.fold_left
      (fun (i, poses, count) line ->
        let l = String.length line in
        let poses = (i, (count, count)) :: poses in
        (i + 1, poses, count + l + 1))
      (1, [], 0) lines
  in
  poses
