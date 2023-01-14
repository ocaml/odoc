#if OCAML_VERSION >= (4, 14, 0)

open Odoc_model.Lang.Locations
open Odoc_model.Lang.Source_code.Info

let pos_of_loc loc = (loc.Location.loc_start.pos_cnum, loc.loc_end.pos_cnum)

let string_of_uid uid = Uid.string_of_uid (Uid.of_shape_uid uid)

module Local_analysis = struct
  let expr poses expr =
    match expr with
    | { Typedtree.exp_desc = Texp_ident (Pident id, _, _); exp_loc; _ }
      when not exp_loc.loc_ghost ->
        let uniq = { anchor = Ident.unique_name id } in
        poses := (Occurence uniq, pos_of_loc exp_loc) :: !poses
    | _ -> ()
  let pat poses (type a) : a Typedtree.general_pattern -> unit = function
    | {
        pat_desc = Tpat_var (id, _stringloc) | Tpat_alias (_, id, _stringloc);
        pat_loc;
        _;
      }
      when not pat_loc.loc_ghost ->
        let uniq = Ident.unique_name id in
        poses := (Def uniq, pos_of_loc pat_loc) :: !poses
    | _ -> ()
end

module Global_analysis = struct
  let init poses uid_to_loc =
    Shape.Uid.Tbl.iter
      (fun uid t ->
        let s = string_of_uid uid in
        poses := (Def s, pos_of_loc t) :: !poses)
      uid_to_loc
  let expr poses uid_to_loc expr =
    match expr with
    | { Typedtree.exp_desc = Texp_ident (_, _, value_description); exp_loc; _ }
      -> (
        match Shape.Uid.Tbl.find_opt uid_to_loc value_description.val_uid with
        | None -> ()
        | Some _ ->
            let uid = { anchor = string_of_uid value_description.val_uid } in
            poses := (Occurence uid, pos_of_loc exp_loc) :: !poses)
    | _ -> ()
end

let of_cmt (cmt : Cmt_format.cmt_infos) =
  let ttree = cmt.cmt_annots in
  match ttree with
  | Cmt_format.Implementation structure ->
      let uid_to_loc = cmt.cmt_uid_to_loc in
      let poses = ref [] in
      Global_analysis.init poses uid_to_loc;
      let expr iterator expr =
        Local_analysis.expr poses expr;
        Global_analysis.expr poses uid_to_loc expr;
        Tast_iterator.default_iterator.expr iterator expr
      in
      let pat iterator pat =
        Local_analysis.pat poses pat;
        Tast_iterator.default_iterator.pat iterator pat
      in
      let iterator = { Tast_iterator.default_iterator with expr; pat } in
      iterator.structure iterator structure;
      !poses
  | _ -> []

#else

let of_cmt _ = []

#endif
