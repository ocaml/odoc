open Cmdliner

let fpath_arg =
  let print ppf v = Fpath.pp ppf v in
  Arg.conv (Fpath.of_string, print)

let odoc_dir =
  let doc = "Directory in which the intermediate odoc files go" in
  Arg.(value & opt fpath_arg (Fpath.v "_odoc/") & info [ "odoc-dir" ] ~doc)

let odocl_dir =
  let doc = "Directory in which the intermediate odocl files go" in
  Arg.(value & opt (some fpath_arg) None & info [ "odocl-dir" ] ~doc)

let index_dir =
  let doc = "Directory in which the index files go" in
  Arg.(value & opt fpath_arg (Fpath.v "_indexes/") & info [ "index-dir" ] ~doc)

let mld_dir =
  let doc = "Directory in which the auto-generated mld files go" in
  Arg.(value & opt fpath_arg (Fpath.v "_mlds/") & info [ "mld-dir" ] ~doc)

let html_dir =
  let doc = "Directory in which the generated HTML files go" in
  Arg.(value & opt fpath_arg (Fpath.v "_html/") & info [ "html-dir" ] ~doc)

let verbose =
  let doc = "Enable verbose output" in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let stats =
  let doc = "Produce 'driver-benchmarks.json' with run stats" in
  Arg.(value & flag & info [ "stats" ] ~doc)

let nb_workers =
  let doc = "Number of workers." in
  Arg.(value & opt int 15 & info [ "j" ] ~doc)

let odoc_bin =
  let doc = "Odoc binary to use" in
  Arg.(value & opt (some string) None & info [ "odoc" ] ~doc)

let compile_grep =
  let doc = "Show compile commands containing the string" in
  Arg.(value & opt (some string) None & info [ "compile-grep" ] ~doc)

let link_grep =
  let doc = "Show link commands containing the string" in
  Arg.(value & opt (some string) None & info [ "link-grep" ] ~doc)

let generate_grep =
  let doc = "Show html-generate commands containing the string" in
  Arg.(value & opt (some string) None & info [ "html-grep" ] ~doc)

let remap =
  let doc = "Remap paths in non-selected packages to ocaml.org" in
  Arg.(value & flag & info [ "remap" ] ~doc)

let index_grep =
  let doc = "Show compile-index commands containing the string" in
  Arg.(value & opt (some string) None & info [ "index-grep" ] ~doc)

let generate_json =
  let doc = "Also generate json output" in
  Arg.(value & flag & info [ "json-output" ] ~doc)

type t = {
  verbose : bool;
  odoc_dir : Fpath.t;
  odocl_dir : Fpath.t option;
  index_dir : Fpath.t;
  mld_dir : Fpath.t;
  html_dir : Fpath.t;
  stats : bool;
  nb_workers : int;
  odoc_bin : string option;
  compile_grep : string option;
  link_grep : string option;
  generate_grep : string option;
  remap : bool;
  index_grep : string option;
  generate_json : bool;
}

let term =
  let open Term in
  let ( let+ ) t f = const f $ t in
  let ( and+ ) a b = const (fun x y -> (x, y)) $ a $ b in
  let+ verbose = verbose
  and+ odoc_dir = odoc_dir
  and+ odocl_dir = odocl_dir
  and+ index_dir = index_dir
  and+ mld_dir = mld_dir
  and+ html_dir = html_dir
  and+ stats = stats
  and+ nb_workers = nb_workers
  and+ odoc_bin = odoc_bin
  and+ compile_grep = compile_grep
  and+ generate_json = generate_json
  and+ link_grep = link_grep
  and+ generate_grep = generate_grep
  and+ index_grep = index_grep
  and+ remap = remap in
  {
    verbose;
    odoc_dir;
    odocl_dir;
    index_dir;
    mld_dir;
    html_dir;
    stats;
    nb_workers;
    odoc_bin;
    compile_grep;
    link_grep;
    generate_grep;
    remap;
    index_grep;
    generate_json;
  }
