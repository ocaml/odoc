open Or_error

let handle_file file ~f =
  Odoc_file.load file
  |> Result.map @@ fun unit' ->
     match unit' with
     | { Odoc_file.content = Unit_content unit; _ } -> Some (f unit)
     | _ -> None

let fold_dirs ~dirs ~f ~init =
  dirs
  |> List.fold_left
       (fun acc dir ->
         acc >>= fun acc ->
         Fs.Directory.fold_files_rec_result ~ext:"odocl"
           (fun acc file ->
             file |> handle_file ~f:(f acc) >>= function
             | None -> Ok acc
             | Some acc -> Ok acc)
           acc dir)
       (Ok init)

module H = Hashtbl.Make (Odoc_model.Paths.Identifier)

let count ~dst ~warnings_options:_ directories =
  let htbl = H.create 100 in
  let f () (unit : Odoc_model.Lang.Compilation_unit.t) =
    let incr tbl p p' =
      let id = Odoc_model.Paths.Path.Resolved.(identifier (p :> t)) in
      let old_value = match H.find_opt tbl id with Some n -> n | None -> 0 in
      if not Odoc_model.Paths.Path.(is_hidden p') then
        H.replace tbl id (old_value + 1)
    in
    let () =
      List.iter
        (function
          | ( Odoc_model.Lang.Source_info.Module
                { documentation = Some (`Resolved p as p'); _ },
              _ ) ->
              incr htbl p Odoc_model.Paths.Path.((p' : Module.t :> t))
          | Value { documentation = Some (`Resolved p as p'); _ }, _ ->
              incr htbl p Odoc_model.Paths.Path.((p' : Value.t :> t))
          | ClassType { documentation = Some (`Resolved p as p'); _ }, _ ->
              incr htbl p Odoc_model.Paths.Path.((p' : ClassType.t :> t))
          | ModuleType { documentation = Some (`Resolved p as p'); _ }, _ ->
              incr htbl p Odoc_model.Paths.Path.((p' : ModuleType.t :> t))
          | Type { documentation = Some (`Resolved p as p'); _ }, _ ->
              incr htbl p Odoc_model.Paths.Path.((p' : Type.t :> t))
          | _ -> ())
        (match unit.source_info with None -> [] | Some i -> i.infos)
    in
    ()
  in
  fold_dirs ~dirs:directories ~f ~init:() >>= fun () ->
  Fs.Directory.mkdir_p (Fs.File.dirname dst);
  let oc = open_out_bin (Fs.File.to_string dst) in
  Marshal.to_channel oc htbl [];
  Ok ()
