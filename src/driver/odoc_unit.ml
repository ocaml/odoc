module Pkg_args = struct
  type t = {
    odoc_dir : Fpath.t;
    odocl_dir : Fpath.t;
    pages : Fpath.t Util.StringMap.t;
    libs : Fpath.t Util.StringMap.t;
  }

  let v ~odoc_dir ~odocl_dir ~pages ~libs =
    let pages, libs = Util.StringMap.(of_list pages, of_list libs) in
    { odoc_dir; odocl_dir; pages; libs }

  let map_rel dir m =
    Util.StringMap.fold (fun a b acc -> (a, Fpath.(dir // b)) :: acc) m []

  let compiled_pages v = map_rel v.odoc_dir v.pages
  let compiled_libs v = map_rel v.odoc_dir v.libs
  let linked_pages v = map_rel v.odocl_dir v.pages
  let linked_libs v = map_rel v.odocl_dir v.libs

  let combine v1 v2 =
    if v1.odoc_dir <> v2.odoc_dir then
      Fmt.invalid_arg "combine: odoc_dir differs";
    if v1.odocl_dir <> v2.odocl_dir then
      Fmt.invalid_arg "combine: odocl_dir differs";
    {
      odoc_dir = v1.odoc_dir;
      odocl_dir = v1.odocl_dir;
      pages = Util.StringMap.union (fun _ x _ -> Some x) v1.pages v2.pages;
      libs = Util.StringMap.union (fun _ x _ -> Some x) v1.libs v2.libs;
    }

  let pp fmt x =
    let sfp_pp =
      Fmt.(
        list ~sep:comma (fun fmt (a, b) ->
            Format.fprintf fmt "(%s, %a)" a Fpath.pp b))
    in
    Format.fprintf fmt
      "@[<hov>odoc_dir: %a@;odocl_dir: %a@;pages: [%a]@;libs: [%a]@]" Fpath.pp
      x.odoc_dir Fpath.pp x.odocl_dir sfp_pp
      (Util.StringMap.bindings x.pages)
      sfp_pp
      (Util.StringMap.bindings x.libs)
end

type sidebar = { output_file : Fpath.t; json : bool; pkg_dir : Fpath.t }

type index = {
  roots : Fpath.t list;
  output_file : Fpath.t;
  json : bool;
  search_dir : Fpath.t;
  sidebar : sidebar option;
}

let pp_index fmt x =
  Format.fprintf fmt
    "@[<hov>roots: %a@;output_file: %a@;json: %b@;search_dir: %a@]"
    (Fmt.list Fpath.pp) x.roots Fpath.pp x.output_file x.json Fpath.pp
    x.search_dir

type 'a unit = {
  parent_id : Odoc.Id.t;
  input_file : Fpath.t;
  output_dir : Fpath.t;
  odoc_file : Fpath.t;
  odocl_file : Fpath.t;
  pkg_args : Pkg_args.t;
  pkgname : string option;
  index : index option;
  enable_warnings : bool;
  to_output : bool;
  kind : 'a;
}

type intf_extra = { hidden : bool; hash : string; deps : intf unit list }

and intf = [ `Intf of intf_extra ]

type impl_extra = { src_id : Odoc.Id.t; src_path : Fpath.t }
type impl = [ `Impl of impl_extra ]

type mld = [ `Mld ]
type md = [ `Md ]
type asset = [ `Asset ]

type all_kinds = [ impl | intf | mld | asset | md ]
type t = all_kinds unit

let rec pp_kind : all_kinds Fmt.t =
 fun fmt x ->
  match x with
  | `Intf x -> Format.fprintf fmt "`Intf %a" pp_intf_extra x
  | `Impl x -> Format.fprintf fmt "`Impl %a" pp_impl_extra x
  | `Mld -> Format.fprintf fmt "`Mld"
  | `Md -> Format.fprintf fmt "`Md"
  | `Asset -> Format.fprintf fmt "`Asset"

and pp_intf_extra fmt x =
  Format.fprintf fmt "@[<hov>hidden: %b@;hash: %s@;deps: [%a]@]" x.hidden x.hash
    Fmt.(list ~sep:comma Fpath.pp)
  @@ List.map (fun x -> x.odoc_file) x.deps

and pp_impl_extra fmt x =
  Format.fprintf fmt "@[<hov>src_id: %s@;src_path: %a@]"
    (Odoc.Id.to_string x.src_id)
    Fpath.pp x.src_path

and pp : all_kinds unit Fmt.t =
 fun fmt x ->
  Format.fprintf fmt
    "@[<hov>parent_id: %s@;\
     input_file: %a@;\
     output_dir: %a@;\
     odoc_file: %a@;\
     odocl_file: %a@;\
     pkg_args: %a@;\
     pkgname: %a@;\
     index: %a@;\
     kind:%a@;\
     @]"
    (Odoc.Id.to_string x.parent_id)
    Fpath.pp x.input_file Fpath.pp x.output_dir Fpath.pp x.odoc_file Fpath.pp
    x.odocl_file Pkg_args.pp x.pkg_args (Fmt.option Fmt.string) x.pkgname
    (Fmt.option pp_index) x.index pp_kind
    (x.kind :> all_kinds)

let pkg_dir pkg = pkg.Packages.pkg_dir
let doc_dir pkg = pkg.Packages.doc_dir
let lib_dir pkg lib = Fpath.(doc_dir pkg / lib.Packages.lib_name)
let src_dir pkg = Fpath.(doc_dir pkg / "src")
let src_lib_dir pkg lib = Fpath.(src_dir pkg / lib.Packages.lib_name)

type dirs = {
  odoc_dir : Fpath.t;
  odocl_dir : Fpath.t;
  index_dir : Fpath.t;
  mld_dir : Fpath.t;
}
