(* Dune build tree *)
open Bos
open Sexplib.Std
[@@@warning "-69-30"]
let dune = ref (Cmd.v "dune")

type item = Library of library

and items = item list

and module_ = {
  name : string;
  impl : string option;
  intf : string option;
  cmt : string option;
  cmti : string option;
}

and library = {
  name : string;
  uid : uid;
  local : bool;
  requires : uid list;
  source_dir : string;
  modules : module_ list;
  include_dirs : string list;
}

and library_list = library list

and uid = string [@@deriving sexp]

(* Eurgh *)
let internal_name_of_library : library -> string option =
 fun l ->
  match l.modules with
  | [] -> None
  | m :: _ -> (
      let ps = List.filter_map (fun x -> x) [ m.cmt; m.cmti ] in
      match ps with
      | [] -> None
      | p :: _ -> (
          let p' = Fpath.relativize ~root:(Fpath.v l.source_dir) (Fpath.v p) in
          match Option.map Fpath.segs p' with
          | Some (objdir :: _ :: _) -> (
              match Astring.String.fields ~is_sep:(fun c -> c = '.') objdir with
              | [ ""; libname; _ ] -> Some libname
              | _ -> None)
          | _ -> None))

let of_dune_describe txt =
  let sexp = Sexplib.Sexp.of_string txt in
  let open Sexplib0.Sexp in
  match sexp with
  | Atom _ -> []
  | List ls ->
      let libs =
        List.filter_map (fun s -> try Some (item_of_sexp s) with _ -> None) ls
      in
      libs
let dune_describe dir =
  let cmd = Cmd.(!dune % "describe" % "--root" % p dir) in
  let out = Worker_pool.submit "dune describe" cmd None in
  match out with Error _ -> [] | Ok out -> of_dune_describe out.Run.output

let of_dune_build dir =
  let contents =
    Bos.OS.Dir.fold_contents ~dotfiles:true
      (fun p acc -> p :: acc)
      []
      Fpath.(dir / "_build" / "default")
  in
  match contents with
  | Error _ -> []
  | Ok c ->
      let libs = dune_describe dir in
      let local_libs =
        List.filter_map
          (function Library l -> if l.local then Some l else None)
          libs
      in
      List.iter
        (fun (lib : library) ->
          Logs.debug (fun m ->
              m "lib %s internal name: %a" lib.name
                Fmt.(option string)
                (internal_name_of_library lib)))
        local_libs;
      let uid_to_libname =
        List.fold_left
          (fun acc l -> Util.StringMap.add l.uid l.name acc)
          Util.StringMap.empty local_libs
      in
      let all_lib_deps =
        List.fold_left
          (fun acc (l : library) ->
            Util.StringMap.add l.name
              (List.filter_map
                 (fun uid -> Util.StringMap.find_opt uid uid_to_libname)
                 l.requires
              |> Util.StringSet.of_list)
              acc)
          Util.StringMap.empty local_libs
      in
      let colon = Fmt.any ":" in
      Format.eprintf "all_lib_deps: %a@."
        Fmt.(list ~sep:comma (pair ~sep:colon string (list ~sep:semi string)))
        (Util.StringMap.to_list all_lib_deps
        |> List.map (fun (x, y) -> (x, Util.StringSet.elements y)));

      (* Format.eprintf "libs: %s@." (Sexplib.Sexp.to_string_hum (sexp_of_library_list local_libs)); *)
      let libname_of_archive =
        List.fold_left
          (fun acc item ->
            match item with
            | Library lib -> (
                let libname_opt = internal_name_of_library lib in
                match libname_opt with
                | Some libname ->
                    let archive =
                      Fpath.(append dir (v lib.source_dir / libname))
                    in
                    Logs.debug (fun m ->
                        m "libname_of_archive: %a -> %s" Fpath.pp archive
                          lib.name);
                    Fpath.Map.add archive lib.name acc
                | None -> acc))
          Fpath.Map.empty libs
      in
      let libs =
        List.filter_map
          (fun (Library lib) ->
            match internal_name_of_library lib with
            | None -> None
            | Some libname ->
                let cmtidir =
                  Fpath.(
                    append dir
                      (v lib.source_dir
                      / Printf.sprintf ".%s.objs" libname
                      / "byte"))
                in
                let id_override =
                  Fpath.relativize
                    ~root:Fpath.(v "_build" / "default")
                    Fpath.(v lib.source_dir)
                  |> Option.map Fpath.to_string
                in
                if List.mem cmtidir c then
                  Some
                    (Packages.Lib.v ~libname_of_archive ~pkg_name:lib.name
                       ~dir:(Fpath.append dir (Fpath.v lib.source_dir))
                       ~cmtidir:(Some cmtidir) ~all_lib_deps ~cmi_only_libs:[]
                       ~id_override)
                else None)
          libs
      in
      let other_docs =
        List.filter_map
          (fun f ->
            if Fpath.has_ext "md" f then
              let md_rel_path =
                Fpath.relativize ~root:Fpath.(v "_build" / "default") f
                |> Option.get
              in
              Some { Packages.md_path = f; md_rel_path }
            else None)
          c
      in
      let libs = List.flatten libs in
      [
          {
            Packages.name = "root";
            version = "1.0";
            libraries = libs;
            mlds = [];
            assets = [];
            selected = true;
            remaps = [];
            pkg_dir = Fpath.v ".";
            doc_dir = Fpath.v ".";
            other_docs;
            config = Global_config.empty;
          };
      ]
   
