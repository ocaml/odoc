let submit desc cmd output_file =
  match Worker_pool.submit desc cmd output_file with
  | Ok x -> x
  | Error exn -> raise exn

let compile_output = ref [ "" ]

let compile_src_output = ref [ "" ]

let link_output = ref [ "" ]

let generate_output = ref [ "" ]

let source_tree_output = ref [ "" ]

let add_prefixed_output cmd list prefix lines =
  if List.length lines > 0 then
    list :=
      !list
      @ (Bos.Cmd.to_string cmd :: List.map (fun l -> prefix ^ ": " ^ l) lines)
