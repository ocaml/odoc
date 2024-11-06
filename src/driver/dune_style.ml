(* Dune build tree *)
open Bos
open Sexplib.Std
[@@@warning "-69-30"]
let dune = ref (Cmd.v "dune")

type item = Library of library

and items = item list

and library = {
  name : string;
  uid : uid;
  local : bool;
  requires : uid list;
  source_dir : string;
  modules : Sexplib.Sexp.t list;
  include_dirs : string list;
}

and library_list = library list

and uid = string [@@deriving sexp]

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
    Bos.OS.Dir.fold_contents ~dotfiles:true (fun p acc -> p :: acc) [] dir
  in
  match contents with
  | Error _ -> Util.StringMap.empty
  | Ok c ->
      let sorted = List.sort (fun p1 p2 -> Fpath.compare p1 p2) c in
      let libs = dune_describe dir in
      let local_libs =
        List.filter_map
          (function Library l -> if l.local then Some l else None)
          libs
      in
      let uid_to_libname =
        List.fold_left
          (fun acc l -> Util.StringMap.add l.uid l.name acc)
          Util.StringMap.empty local_libs
      in
      let all_lib_deps =
        List.fold_left
          (fun acc l ->
            Util.StringMap.add l.name
              (List.filter_map
                 (fun uid -> Util.StringMap.find_opt uid uid_to_libname)
                 l.requires
              |> Util.StringSet.of_list)
              acc)
          Util.StringMap.empty local_libs
      in
      (* Format.eprintf "all_lib_deps: %a@." Fmt.(list ~sep:comma (pair string (list ~sep:semi string))) (Util.StringMap.to_list all_lib_deps); *)
      (* Format.eprintf "libs: %s@." (Sexplib.Sexp.to_string_hum (sexp_of_library_list local_libs)); *)
      let libs =
        List.filter_map
          (fun x ->
            match Fpath.segs x |> List.rev with
            | "byte" :: libname :: path ->
                let sz = String.length ".objs" in
                if
                  Astring.String.is_suffix ~affix:".objs" libname
                  && String.length libname > sz + 1
                  && libname.[0] = '.'
                then
                  let libname =
                    String.sub libname 1 (String.length libname - sz - 1)
                  in
                  Some
                    (libname, Fpath.(v (String.concat dir_sep (List.rev path))))
                else None
            | _ -> None)
          sorted
      in
      let libname_of_archive =
        List.fold_left
          (fun acc (libname, path) ->
            Fpath.Map.add Fpath.(path / libname) libname acc)
          Fpath.Map.empty libs
      in
      let libs =
        List.map
          (fun (libname, path) ->
            let cmtidir =
              Fpath.(path / Printf.sprintf ".%s.objs" libname / "byte")
            in
            let pkg_dir = Fpath.rem_prefix dir path |> Option.get in
            ( pkg_dir,
              Packages.Lib.v ~libname_of_archive ~pkg_name:libname ~dir:path
                ~cmtidir:(Some cmtidir) ~all_lib_deps ~cmi_only_libs:[] ))
          libs
      in
      let packages =
        List.filter_map
          (fun (pkg_dir, lib) ->
            match lib with
            | [ lib ] ->
                Some
                  ( lib.Packages.lib_name,
                    {
                      Packages.name = lib.Packages.lib_name;
                      version = "1.0";
                      libraries = [ lib ];
                      mlds = [];
                      assets =
                        []
                        (* When dune has a notion of doc assets, do something *);
                      enable_warnings = false;
                      pkg_dir;
                      other_docs = Fpath.Set.empty;
                      config = Global_config.empty;
                    } )
            | _ -> None)
          libs
      in
      Util.StringMap.of_list packages
