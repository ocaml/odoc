let string_of_uid uid =
  match uid with
  | Shape.Uid.Compilation_unit s -> s
  | Item { comp_unit; id } -> comp_unit ^ string_of_int id
  | Predef s -> s
  | _ -> "rien"

let pos_of_loc loc = (loc.Location.loc_start.pos_cnum, loc.loc_end.pos_cnum)

module Local_analysis = struct
  let expr poses expr =
    match expr with
    | { Typedtree.exp_desc = Texp_ident (id, _, _); exp_loc; _ } ->
        let extract_id id =
          match id with
          | Path.Pident id ->
              let uniq = Ident.unique_name id in
              poses := (Types.Occurence uniq, pos_of_loc exp_loc) :: !poses
          | _ -> ()
        in
        extract_id id
    | _ -> ()
  let pat poses (type a) : a Typedtree.general_pattern -> unit = function
    | {
        Typedtree.pat_desc =
          ( Typedtree.Tpat_var (id, _stringloc)
          | Typedtree.Tpat_alias (_, id, _stringloc) );
        pat_loc;
        _;
      } ->
        let uniq = Ident.unique_name id in
        poses := (Types.Def uniq, pos_of_loc pat_loc) :: !poses
    | _ -> ()
end

module Global_analysis = struct
  let init poses uid_to_loc =
    Shape.Uid.Tbl.iter
      (fun uid t ->
        let s = string_of_uid uid in
        poses := (Types.Def s, pos_of_loc t) :: !poses)
      uid_to_loc
  let expr poses uid_to_loc expr =
    match expr with
    | { Typedtree.exp_desc = Texp_ident (_, _, value_description); exp_loc; _ }
      -> (
        match Shape.Uid.Tbl.find_opt uid_to_loc value_description.val_uid with
        | None -> ()
        | Some _ ->
            poses :=
              ( Types.Occurence (string_of_uid value_description.val_uid),
                pos_of_loc exp_loc )
              :: !poses)
    | _ -> ()
end

let jmp_to_def_locs (cmt : Cmt_format.cmt_infos) =
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

let jmp_to_def cmt =
  jmp_to_def_locs cmt |> List.map (fun (x, y) -> (Types.Local_jmp x, y))
