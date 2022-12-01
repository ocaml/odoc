let highlight src =
  Syntax_highlighter.syntax_highlighting_locs src
  |> List.rev_map (fun (x, y) -> (Types.Token x, y))
(* The order won't matter and input can be large *)
