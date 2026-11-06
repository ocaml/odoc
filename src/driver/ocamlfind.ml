(* The standard library of the compiler that built the units: [CAMLLIB], or
   what [ocamlc -where] prints. Findlib's own default is the one it was built
   against, which is the wrong one when the driver comes from another switch,
   and the configuration file need not name it. *)
let stdlib =
  lazy
    (match Sys.getenv_opt "CAMLLIB" with
    | Some d when d <> "" -> Some d
    | _ -> (
        match
          Bos.OS.Cmd.(run_out Bos.Cmd.(v "ocamlc" % "-where") |> to_string)
        with
        | Ok w -> Some (String.trim w)
        | Error _ -> None))

(* Directories searched before the configured ones: in voodoo mode, the
   [lib] of the package being documented, which voodoo-prep copied out of the
   switch. Set before the first query. *)
let extra_path = ref []

(* Findlib reads a configuration file that ocamlfind installs, and it need not
   be there. ocaml-docs-ci documents each package in a switch holding only
   that package's dependency closure, and a closure of dune-only packages
   contains no ocamlfind. Findlib is then set up by hand, with the search path
   the configuration would have given: [OCAMLPATH], the standard library's
   directory, which holds the METAs of the compiler's own libraries, and the
   switch's [lib].

   The configuration file is the switch's, when opam's environment selects a
   switch and [OCAMLFIND_CONF] does not name another: [etc/findlib.conf] if
   there is one, else [lib/findlib.conf]. Findlib looks there itself only when
   ocamlfind was configured with relative paths, which opam's is not. Its own
   default is the one in the switch it was built in. *)
let config =
  match
    (Sys.getenv_opt "OCAMLFIND_CONF", Sys.getenv_opt "OPAM_SWITCH_PREFIX")
  with
  | None, Some prefix ->
      let conf dir = Filename.(concat (concat prefix dir) "findlib.conf") in
      Some (if Sys.file_exists (conf "etc") then conf "etc" else conf "lib")
  | _ -> None

let setup =
  lazy
    (let stdlib = Lazy.force stdlib in
     let ocamlpath =
       match Sys.getenv_opt "OCAMLPATH" with
       | None -> []
       | Some p -> Fl_split.path p
     in
     let ocamlpath = List.rev !extra_path @ ocamlpath in
     let env_ocamlpath =
       String.concat (if Sys.win32 then ";" else ":") ocamlpath
     in
     try Findlib.init ?config ~env_ocamlpath ?env_camllib:stdlib ()
     with e ->
       Logs.debug (fun m ->
           m "No findlib configuration (%s), so findlib is set up by hand"
             (Printexc.to_string e));
       let lib = Filename.concat (Opam.prefix ()) "lib" in
       Findlib.init_manually ?stdlib ~install_dir:lib ~meta_dir:""
         ~search_path:(ocamlpath @ Option.to_list stdlib @ [ lib ])
         ())

let init () = Lazy.force setup

let add_path dir =
  if Lazy.is_val setup then invalid_arg "Ocamlfind.add_path: findlib is set up";
  extra_path := Fpath.to_string dir :: !extra_path

(* The libraries a META file defines, read by findlib: the library named
   after the file, [<name>/META] or [META.<name>], and its sub-libraries. *)
let libraries_of_meta meta =
  let name =
    if Fpath.basename meta = "META" then Fpath.(basename (parent meta))
    else
      Fpath.get_ext ~multi:false meta |> fun e ->
      String.sub e 1 (String.length e - 1)
  in
  let dir = Fpath.(to_string (rem_empty_seg (parent meta))) in
  Fl_package_base.packages_in_meta_file ~name ~dir
    ~meta_file:(Fpath.to_string meta) ()
  |> List.map (fun (p : Fl_package_base.package) -> p.package_name)
  |> List.filter (fun n ->
         not (List.mem "__private__" (String.split_on_char '.' n)))

let all () =
  init ();
  Fl_package_base.list_packages ()

(* Where a library's files are. A library findlib has never heard of is
   routine: an optional dependency named in a META [requires]. *)
let get_dir lib =
  init ();
  try
    Fl_package_base.query lib |> fun x ->
    Ok Fpath.(v x.package_dir |> to_dir_path)
  with e ->
    Logs.debug (fun m ->
        m "No findlib directory for '%s': %s" lib (Printexc.to_string e));
    Error (`Msg "Error getting directory")

let archives pkg =
  match pkg with
  | "stdlib" -> [ "stdlib.cma"; "stdlib.cmxa" ]
  | _ -> (
      init ();
      match Fl_package_base.query pkg with
      | exception e ->
          Logs.debug (fun m ->
              m "No findlib archives for '%s': %s" pkg (Printexc.to_string e));
          []
      | package -> (
          let get_1 preds =
            try
              [
                Fl_metascanner.lookup "archive" preds
                  package.Fl_package_base.package_defs;
              ]
            with _ -> []
          in
          let found preds_list =
            List.concat_map get_1 preds_list
            |> List.filter (fun x -> String.length x > 0)
            |> List.sort_uniq String.compare
          in
          (* A toplevel add-on, such as num-top, declares its archive for the
             toplevel only. That archive is looked for only when there is no
             other: a library may have a separate one for the toplevel. *)
          match
            found
              [
                [ "native" ];
                [ "byte" ];
                [ "native"; "ppx_driver" ];
                [ "byte"; "ppx_driver" ];
              ]
          with
          | [] -> found [ [ "byte"; "toploop" ]; [ "native"; "toploop" ] ]
          | archives -> archives))

(* The libraries a library requires directly: its META [requires] field. The
   field is read as written rather than resolved, because
   [Fl_package_base.requires] fails outright when an optional dependency such
   as [faraday-async] is not installed, which would lose the library's other
   dependencies too.

   It is read under the [ppx_driver] predicate, as the compiler does when
   building against the library. Without it, the [requires(-ppx_driver)]
   stanzas of ppx libraries would add the ppx runner ([ppx_deriving], say) to
   the dependencies of a library that merely offers a rewriter; those stanzas
   are for programs using the rewriter, not for the library's modules. *)
let direct_deps pkg =
  init ();
  try
    let package = Fl_package_base.query pkg in
    let requires =
      try Fl_metascanner.lookup "requires" [ "ppx_driver" ] package.package_defs
      with Not_found -> ""
    in
    Ok
      (Util.StringSet.add "stdlib"
         (Util.StringSet.of_list (Fl_split.in_words requires)))
  with e -> Error (`Msg (Printexc.to_string e))
