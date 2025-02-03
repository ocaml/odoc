open Cmdliner

let fpath_arg =
  let print ppf v = Fpath.pp ppf v in
  Arg.conv (Fpath.of_string, print)

let html_dir =
  let doc = "Directory in which the generated HTML files go" in
  Arg.(
    value
    & opt fpath_arg (Fpath.v "_html/")
    & info [ "html-dir" ] ~doc ~docs:Manpage.s_common_options)

let verbose =
  let doc = "Enable verbose output" in
  Arg.(
    value & flag & info [ "v"; "verbose" ] ~doc ~docs:Manpage.s_common_options)

let stats =
  let doc = "Produce 'driver-benchmarks.json' with run stats" in
  Arg.(value & flag & info [ "stats" ] ~doc ~docs:Manpage.s_common_options)

let nb_workers =
  let doc = "Number of workers." in
  Arg.(
    value
    & opt int (Domain.recommended_domain_count () - 1)
    & info [ "j" ] ~doc ~docs:Manpage.s_common_options)

let odoc_bin =
  let doc = "Odoc binary to use" in
  Arg.(
    value
    & opt (some string) None
    & info [ "odoc" ] ~doc ~docs:Manpage.s_common_options)

let odoc_md_bin =
  let doc = "Odoc-md binary to use" in
  Arg.(
    value
    & opt (some string) None
    & info [ "odoc-md" ] ~doc ~docs:Manpage.s_common_options)

let generate_json =
  let doc = "Also generate json output" in
  Arg.(
    value & flag & info [ "json-output" ] ~doc ~docs:Manpage.s_common_options)

let odoc_dir =
  let doc = "Directory in which the intermediate odoc files go" in
  Arg.(value & opt (some fpath_arg) None & info [ "odoc-dir" ] ~doc)

let odocl_dir =
  let doc = "Directory in which the intermediate odocl files go" in
  Arg.(value & opt (some fpath_arg) None & info [ "odocl-dir" ] ~doc)

let index_dir =
  let doc = "Directory in which the intermediate index files go" in
  Arg.(value & opt (some fpath_arg) None & info [ "index-dir" ] ~doc)

let mld_dir =
  let doc = "Directory in which the auto-generated mld files go" in
  Arg.(value & opt (some fpath_arg) None & info [ "mld-dir" ] ~doc)

type t = {
  verbose : bool;
  html_dir : Fpath.t;
  stats : bool;
  nb_workers : int;
  odoc_bin : string option;
  odoc_md_bin : string option;
  generate_json : bool;
}

type dirs = {
  odoc_dir : Fpath.t option;
  odocl_dir : Fpath.t option;
  mld_dir : Fpath.t option;
  index_dir : Fpath.t option;
}

let with_dirs dirs fn : unit =
  let with_dir = Util.with_dir in
  let { odoc_dir; odocl_dir; mld_dir; index_dir } = dirs in
  with_dir odoc_dir "odoc-%s" @@ fun odoc_dir () ->
  with_dir odocl_dir "odocl-%s" @@ fun odocl_dir () ->
  with_dir index_dir "index-%s" @@ fun index_dir () ->
  with_dir mld_dir "mld-%s" @@ fun mld_dir () ->
  fn ~odoc_dir ~odocl_dir ~index_dir ~mld_dir ()

open Term

let ( let+ ) t f = const f $ t
let ( and+ ) a b = const (fun x y -> (x, y)) $ a $ b

let dirs_term =
  let+ odoc_dir = odoc_dir
  and+ odocl_dir = odocl_dir
  and+ mld_dir = mld_dir
  and+ index_dir = index_dir in
  { odoc_dir; odocl_dir; mld_dir; index_dir }

let term =
  let+ verbose = verbose
  and+ html_dir = html_dir
  and+ stats = stats
  and+ nb_workers = nb_workers
  and+ odoc_bin = odoc_bin
  and+ odoc_md_bin = odoc_md_bin
  and+ generate_json = generate_json in
  { verbose; html_dir; stats; nb_workers; odoc_bin; odoc_md_bin; generate_json }
