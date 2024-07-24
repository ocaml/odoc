(* Dune build tree *)

(* Map lib names to package names. *)
let mk_pkgname ~dir libname path =
  let p_dir = Fpath.rem_prefix dir path |> Option.get in
  { Packages.p_name = libname; p_dir }

let of_dune_build dir =
  let contents =
    Bos.OS.Dir.fold_contents ~dotfiles:true (fun p acc -> p :: acc) [] dir
  in
  match contents with
  | Error _ -> Util.StringMap.empty
  | Ok c ->
      let sorted = List.sort (fun p1 p2 -> Fpath.compare p1 p2) c in
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
      let libs =
        List.map
          (fun (libname, path) ->
            let cmtidir =
              Fpath.(path / Printf.sprintf ".%s.objs" libname / "byte")
            in
            let pkgname = mk_pkgname ~dir libname path in
            ( pkgname,
              Packages.Lib.v ~pkgname
                ~libname_of_archive:(Util.StringMap.singleton libname libname)
                ~dir:path ~cmtidir:(Some cmtidir) ))
          libs
      in
      let packages =
        List.filter_map
          (fun (pkgname, lib) ->
            match lib with
            | [ lib ] ->
                (* Accept only one library per package for now. *)
                Some
                  ( lib.Packages.lib_name,
                    {
                      Packages.version = "1.0";
                      libraries = [ lib ];
                      mlds = [];
                      mld_odoc_dir = Fpath.v lib.Packages.lib_name;
                      pkgname;
                      other_docs = Fpath.Set.empty;
                    } )
            | _ -> None)
          libs
      in
      Util.StringMap.of_list packages
