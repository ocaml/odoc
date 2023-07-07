#if OCAML_VERSION >= (4, 14, 0)

open Odoc_model.Lang.Source_info

let pos_of_loc loc = (loc.Location.loc_start.pos_cnum, loc.loc_end.pos_cnum)

let ( let= ) m f = match m with Some x -> f x | None -> ()

type annotations =
  | LocalDef of string * string
  | LocalJmp of string
  | Def of Odoc_model.Names.DefName.t
  | DefJmp of Odoc_model.Names.DefName.t

module Analysis = struct
  (** Generate the anchors that will be pointed to by [lookup_def]. *)
  let init lookup_def_t poses uid_to_loc =
    Shape.Uid.Tbl.iter
      (fun uid t ->
        let= s = Lookup_def.anchor_of_uid lookup_def_t uid in
        poses := (Def s, pos_of_loc t) :: !poses)
      uid_to_loc

  let pat poses (type a) : a Typedtree.general_pattern -> unit = function
    | {
        pat_desc = Tpat_var (id, _stringloc) | Tpat_alias (_, id, _stringloc);
        pat_loc;
        _;
      }
        when not pat_loc.loc_ghost ->
          let uniq = Ident.unique_name id in
          let name = Ident.name id in
          poses := (LocalDef (uniq, name), pos_of_loc pat_loc) :: !poses
    | _ -> ()
  
  let expr poses lookup_def_t expr =
    match expr with
    | { Typedtree.exp_desc = Texp_ident (p, _, value_description); exp_loc; _ }
      -> (
        (* Only generate anchor if the uid is in the location table. We don't
           link to modules outside of the compilation unit. *)
        match Lookup_def.anchor_of_uid lookup_def_t value_description.val_uid with
        | Some anchor ->
          poses := (DefJmp anchor, pos_of_loc exp_loc) :: !poses
        | None ->
          match p with
          | Pident id
          when not exp_loc.loc_ghost ->
            let anchor = Ident.unique_name id in
            poses := (LocalJmp anchor, pos_of_loc exp_loc) :: !poses
          | _ -> ())
    | _ -> ()
end


let postprocess_poses poses =
  let local_def_anchors =
    List.filter_map (function
      | LocalDef (uniq,name), (start,_) ->
        Some (uniq, Odoc_model.Names.LocalName.make_std (Printf.sprintf "local_%s_%d" name start))
      | _ -> None) poses in
  List.filter_map (function
    | LocalDef (uniq,_), loc -> Some (Odoc_model.Lang.Source_info.LocalDef (List.assoc uniq local_def_anchors), loc)
    | LocalJmp uniq, loc -> (
      match List.assoc_opt uniq local_def_anchors with
      | Some anchor -> Some (LocalOccurence anchor, loc)
      | None -> None)
    | Def x, loc -> Some (Def x, loc)
    | DefJmp x, loc -> Some (Occurence x, loc)) poses 

let of_cmt lookup_def_t (cmt : Cmt_format.cmt_infos) =
  let ttree = cmt.cmt_annots in
  match ttree with
  | Cmt_format.Implementation structure ->
      let uid_to_loc = cmt.cmt_uid_to_loc in
      let poses = ref [] in
      Analysis.init lookup_def_t poses uid_to_loc;
      let expr iterator expr =
        Analysis.expr poses lookup_def_t expr;
        Tast_iterator.default_iterator.expr iterator expr
      in
      let pat iterator pat =
        Analysis.pat poses pat;
        Tast_iterator.default_iterator.pat iterator pat
      in
      let iterator = { Tast_iterator.default_iterator with expr; pat } in
      iterator.structure iterator structure;
      postprocess_poses !poses
  | _ -> []

#else

let of_cmt _ _ = []

#endif
