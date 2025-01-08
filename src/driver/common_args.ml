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
  Arg.(value & opt int 15 & info [ "j" ] ~doc ~docs:Manpage.s_common_options)

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

type t = {
  verbose : bool;
  html_dir : Fpath.t;
  stats : bool;
  nb_workers : int;
  odoc_bin : string option;
  odoc_md_bin : string option;
  generate_json : bool;
}

let term =
  let open Term in
  let ( let+ ) t f = const f $ t in
  let ( and+ ) a b = const (fun x y -> (x, y)) $ a $ b in
  let+ verbose = verbose
  and+ html_dir = html_dir
  and+ stats = stats
  and+ nb_workers = nb_workers
  and+ odoc_bin = odoc_bin
  and+ odoc_md_bin = odoc_md_bin
  and+ generate_json = generate_json in
  { verbose; html_dir; stats; nb_workers; odoc_bin; odoc_md_bin; generate_json }
