(* Dune build tree *)
open Bos
open Sexplib.Std
[@@@warning "-69-30"]

let monorepo_pkg_name = "__pkg__"

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
let internal_name_of_library : library -> (string * Fpath.t) option =
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
          | Some (objdir :: "byte" :: _) -> (
              (* cmt files are expected to be in [library_path/.libname.objs/byte/name.cmt]. *)
              match Astring.String.fields ~is_sep:(fun c -> c = '.') objdir with
              | [ ""; libname; "objs" ] ->
                  Some (libname, Fpath.(parent (v p) |> rem_empty_seg))
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

let of_dune_build dir ~extra_pkgs ~extra_libs =
  let root = Fpath.(dir / "_build" / "default") in
  let contents =
    Bos.OS.Dir.fold_contents ~dotfiles:true (fun p acc -> p :: acc) [] root
  in
  match contents with
  | Error _ -> []
  | Ok c ->
      let cset = Fpath.Set.of_list c in
      let libs = dune_describe dir in

      let local_libs, global_libs =
        List.partition
          (function l -> l.local)
          (List.filter_map (function Library l -> Some l) libs)
      in

      List.iter
        (fun (lib : library) ->
          Logs.debug (fun m ->
              m "lib %s internal name: (%a)" lib.name
                Fmt.(option (pair string Fpath.pp))
                (internal_name_of_library lib)))
        local_libs;

      let uid_to_libname =
        List.fold_left
          (fun acc l -> Util.StringMap.add l.uid l.name acc)
          Util.StringMap.empty (local_libs @ global_libs)
      in

      let all_lib_deps =
        List.fold_left
          (fun acc (l : library) ->
            let libs =
              List.filter_map
                (fun uid -> Util.StringMap.find_opt uid uid_to_libname)
                l.requires
            in
            let libs = if l.name = "stdlib" then libs else "stdlib" :: libs in
            Util.StringMap.add l.name (libs |> Util.StringSet.of_list) acc)
          Util.StringMap.empty (local_libs @ global_libs)
      in

      let rec with_trans_deps =
        let cache = Hashtbl.create (List.length libs) in
        fun lib_name ->
          try Hashtbl.find cache lib_name
          with Not_found ->
            let libs =
              try Util.StringMap.find lib_name all_lib_deps
              with Not_found ->
                Logs.debug (fun m -> m "No lib deps for library %s" lib_name);
                Util.StringSet.empty
            in
            let result =
              Util.StringSet.fold
                (fun l acc -> Util.StringSet.union (with_trans_deps l) acc)
                libs libs
            in
            Hashtbl.add cache lib_name result;
            result
      in

      let all_lib_deps =
        Util.StringMap.mapi
          (fun lib_name _ -> with_trans_deps lib_name)
          all_lib_deps
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
                | Some (libname, _) ->
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
          (fun lib ->
            match internal_name_of_library lib with
            | None -> None
            | Some (libname, cmtidir) ->
                let cmtidir = Fpath.(append dir cmtidir) in
                let id_override =
                  Fpath.relativize
                    ~root:Fpath.(v "_build/default")
                    Fpath.(v lib.source_dir)
                  |> Option.map Fpath.to_string
                in
                (match id_override with
                | None ->
                    Logs.warn (fun m ->
                        m "Could not determine id_override for library '%s'"
                          libname)
                | _ -> ());
                if Fpath.Set.mem cmtidir cset then
                  Some
                    (Packages.Lib.v ~libname_of_archive ~pkg_name:lib.name
                       ~dir:(Fpath.append dir (Fpath.v lib.source_dir))
                       ~cmtidir:(Some cmtidir) ~all_lib_deps ~cmi_only_libs:[]
                       ~id_override)
                else None)
          local_libs
      in
      let find_docs ext =
        List.filter_map
          (fun f ->
            if Fpath.has_ext ext f then
              let rel_path = Fpath.relativize ~root f |> Option.get in
              Some (f, rel_path)
            else None)
          c
      in
      let other_docs =
        find_docs ".md"
        |> List.map (fun (p, r) -> { Packages.md_path = p; md_rel_path = r })
      in
      let mlds =
        find_docs ".mld"
        |> List.map (fun (p, r) -> { Packages.mld_path = p; mld_rel_path = r })
      in
      let assets =
        find_docs ".jpg"
        |> List.map (fun (p, r) ->
               { Packages.asset_path = p; asset_rel_path = r })
      in
      let libs = List.flatten libs in
      let global_config =
        {
          Global_config.deps =
            {
              packages = extra_pkgs;
              libraries =
                extra_libs
                @ List.map (fun (lib : Packages.libty) -> lib.lib_name) libs;
            };
        }
      in
      let local =
        [
          {
            Packages.name = monorepo_pkg_name;
            version = "1.0";
            libraries = libs;
            mlds;
            assets;
            selected = true;
            remaps = [];
            pkg_dir = Fpath.v ".";
            doc_dir = Fpath.v ".";
            other_docs;
            config = global_config;
          };
        ]
      in
      let global =
        Packages.of_libs
          ~packages_dir:(Some (Fpath.v "opam_switch"))
          (List.map (fun (l : library) -> l.name) global_libs
          |> Util.StringSet.of_list)
      in
      local @ global
