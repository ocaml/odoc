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

module Occtbl : sig
  type item = { direct : int; indirect : int; sub : item H.t }
  type t = item H.t
  type key = Odoc_model.Paths.Identifier.t
  val v : unit -> t

  val add : t -> key -> unit

  val iter : (key -> item -> unit) -> t -> unit

  val get : t -> key -> item option
end = struct
  type item = { direct : int; indirect : int; sub : item H.t }
  type t = item H.t
  type key = Odoc_model.Paths.Identifier.t

  let v_item () = { direct = 0; indirect = 0; sub = H.create 0 }

  let v () = H.create 0

  let add tbl id =
    let rec add ?(kind = `Indirect) id =
      let incr htbl id =
        let { direct; indirect; sub } =
          match H.find_opt htbl id with Some n -> n | None -> v_item ()
        in
        let direct, indirect =
          match kind with
          | `Direct -> (direct + 1, indirect)
          | `Indirect -> (direct, indirect + 1)
        in
        H.replace htbl id { direct; indirect; sub };
        sub
      in
      let do_ parent =
        let htbl = add (parent :> key) in
        incr htbl id
      in
      match id.iv with
      | `InstanceVariable (parent, _) -> do_ parent
      | `Parameter (parent, _) -> do_ parent
      | `Module (parent, _) -> do_ parent
      | `ModuleType (parent, _) -> do_ parent
      | `Method (parent, _) -> do_ parent
      | `Field (parent, _) -> do_ parent
      | `Extension (parent, _) -> do_ parent
      | `Type (parent, _) -> do_ parent
      | `CoreType _ -> incr tbl id
      | `Constructor (parent, _) -> do_ parent
      | `Exception (parent, _) -> do_ parent
      | `ExtensionDecl (parent, _, _) -> do_ parent
      | `Class (parent, _) -> do_ parent
      | `Value (parent, _) -> do_ parent
      | `ClassType (parent, _) -> do_ parent
      | `Root _ -> incr tbl id
      | `SourcePage _ | `Page _ | `LeafPage _ | `SourceLocation _
      | `CoreException _ | `Label _ | `SourceLocationMod _ | `Result _
      | `AssetFile _ | `SourceDir _ | `SourceLocationInternal _ ->
          assert false
    in
    let _htbl = add ~kind:`Direct id in
    ()

  let rec get t id =
    let ( >>= ) = Option.bind in
    let do_ parent =
      get t (parent :> key) >>= fun { sub; _ } -> H.find_opt sub id
    in
    match id.iv with
    | `InstanceVariable (parent, _) -> do_ parent
    | `Parameter (parent, _) -> do_ parent
    | `Module (parent, _) -> do_ parent
    | `ModuleType (parent, _) -> do_ parent
    | `Method (parent, _) -> do_ parent
    | `Field (parent, _) -> do_ parent
    | `Extension (parent, _) -> do_ parent
    | `ExtensionDecl (parent, _, _) -> do_ parent
    | `Type (parent, _) -> do_ parent
    | `Constructor (parent, _) -> do_ parent
    | `Exception (parent, _) -> do_ parent
    | `Class (parent, _) -> do_ parent
    | `Value (parent, _) -> do_ parent
    | `ClassType (parent, _) -> do_ parent
    | `Root _ -> H.find_opt t id
    | `SourcePage _ | `Page _ | `LeafPage _ | `CoreType _ | `SourceLocation _
    | `CoreException _ | `Label _ | `SourceLocationMod _ | `Result _
    | `AssetFile _ | `SourceDir _ | `SourceLocationInternal _ ->
        assert false

  let rec iter f tbl =
    H.iter
      (fun id v ->
        iter f v.sub;
        f id v)
      tbl
end

let count ~dst ~warnings_options:_ directories include_hidden include_persistent
    =
  let htbl = H.create 100 in
  let f () (unit : Odoc_model.Lang.Compilation_unit.t) =
    let incr tbl p persistent =
      let p = (p :> Odoc_model.Paths.Path.Resolved.t) in
      let id = Odoc_model.Paths.Path.Resolved.identifier p in
      if (not (Odoc_model.Paths.Path.Resolved.is_hidden p)) || include_hidden
      then if (not persistent) || include_persistent then Occtbl.add tbl id
    in
    let () =
      List.iter
        (function
          | ( Odoc_model.Lang.Source_info.Module
                { documentation = Some (`Resolved p, persistent); _ },
              _ ) ->
              incr htbl p persistent
          | Value { documentation = Some (`Resolved p, persistent); _ }, _ ->
              incr htbl p persistent
          | ClassType { documentation = Some (`Resolved p, persistent); _ }, _
            ->
              incr htbl p persistent
          | ModuleType { documentation = Some (`Resolved p, persistent); _ }, _
            ->
              incr htbl p persistent
          | Type { documentation = Some (`Resolved p, persistent); _ }, _ ->
              incr htbl p persistent
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

open Astring
open Or_error

let parse_input_file input =
  let is_sep = function '\n' | '\r' -> true | _ -> false in
  Fs.File.read input >>= fun content ->
  let files =
    String.fields ~empty:false ~is_sep content |> List.rev_map Fs.File.of_string
  in
  Ok files

let parse_input_files input =
  List.fold_left
    (fun acc file ->
      acc >>= fun acc ->
      parse_input_file file >>= fun files -> Ok (files :: acc))
    (Ok []) input
  >>= fun files -> Ok (List.concat files)

let aggregate files file_list ~warnings_options:_ ~dst =
  parse_input_files file_list >>= fun new_files ->
  let files = files @ new_files in
  let from_file file : Occtbl.t =
    let ic = open_in_bin (Fs.File.to_string file) in
    Marshal.from_channel ic
  in
  let rec loop n f =
    if n > 0 then (
      f ();
      loop (n - 1) f)
    else ()
  in
  let occtbl =
    match files with
    | [] -> H.create 0
    | file1 :: files ->
        let acc = from_file file1 in
        List.iter
          (fun file ->
            Occtbl.iter
              (fun id { direct; _ } ->
                loop direct (fun () -> Occtbl.add acc id))
              (from_file file))
          files;
        acc
  in
  let oc = open_out_bin (Fs.File.to_string dst) in
  Marshal.to_channel oc occtbl [];
  Ok ()
