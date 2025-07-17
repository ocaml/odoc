module Table = Table

let of_impl ~include_hidden unit htbl deftbl =
  let incr tbl p src_loc =
    let open Odoc_model.Paths.Path.Resolved in
    let p = (p :> t) in
    let id = identifier p in
    match id with
    | Some id when (not (is_hidden p)) || include_hidden -> Table.add tbl id src_loc
    | _ -> ()
  in
  let add_item p implementation =
    let src_loc =
      match implementation with
      | Some (Odoc_model.Lang.Source_info.Resolved impl) ->
          Table.Deftbl.get deftbl impl
      | _ -> None
    in
    incr htbl p src_loc
  in
  let open Odoc_model.Lang in
  List.iter
    (function
      | Source_info.Module { documentation = Some (`Resolved p); implementation }, _ ->
          add_item p implementation
      | Value { documentation = Some (`Resolved p); implementation }, _ ->
          add_item p implementation
      | ModuleType { documentation = Some (`Resolved p); implementation }, _ ->
          add_item p implementation
      | Type { documentation = Some (`Resolved p); implementation }, _ ->
          add_item p implementation
      | Definition _, _ -> ()
      | _ -> ())
    unit.Implementation.source_info

let aggregate ~tbl ~data = Table.merge_into ~src:data ~dst:tbl

type hg_revision = string

let unspecified_hg_revision : hg_revision = ""

(* TODO: The [revision] field is really a temporary hack and should be replaced
   by a dummy with an opaque type. Annoyingly, just removing the field would
   risk causing segfaults due to unmarshalling.
*)
type t = {table : Table.t; revision: hg_revision; max_occurrences: int}

let from_file file : t =
  Odoc_utils.Io_utils.unmarshal (Fpath.to_string file)
;;

(* FIXME: Copied from [Odoc_odoc.Fs], which should probably just all be moved into
   [Odoc_utils]. *)
let mkdir_p dir =
  let open StdLabels in
  let mkdir d =
    try Unix.mkdir (Fpath.to_string d) 0o755 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    | exn -> raise exn
  in
  let rec dirs_to_create p acc =
    if Sys.file_exists (Fpath.to_string p) then acc
    else dirs_to_create (Fpath.parent p) (p :: acc)
  in
  List.iter (dirs_to_create dir []) ~f:mkdir
;;

let to_file (t : t) file =
  mkdir_p (Fpath.parent file);
  Odoc_utils.Io_utils.marshal (Fpath.to_string file) t;
;;

let from_occtbl occtbl revision : t =
  let get_max tbl =
    let max = ref 0 in
    let f _key (item : Table.item) =
      max := Int.max (!max) (item.direct + item.indirect)
    in
    Table.iter f tbl; !max
  in
  let revision = revision |> Option.value ~default:unspecified_hg_revision in
  {table = occtbl; max_occurrences = get_max occtbl; revision}
