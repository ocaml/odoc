type log_dest =
  [ `Compile
  | `Compile_src
  | `Link
  | `Count_occurrences
  | `Generate
  | `Index
  | `Source_tree
  | `Sherlodoc
  | `Classify ]

let outputs : (log_dest * [ `Out | `Err ] * string * string) list ref = ref []

let maybe_log log_dest r =
  match log_dest with
  | Some (dest, prefix) ->
      let add ty s = outputs := !outputs @ [ (dest, ty, prefix, s) ] in
      add `Out r.Run.output;
      add `Err r.Run.errors
  | None -> ()

let submit log_dest desc cmd output_file =
  match Worker_pool.submit desc cmd output_file with
  | Ok x ->
      maybe_log log_dest x;
      String.split_on_char '\n' x.output
  | Error exn -> raise exn
