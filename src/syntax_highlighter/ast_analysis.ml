type description = Jmp_to_def of string | Def of string

let string_of_uid uid =
  match uid with
  | Shape.Uid.Compilation_unit s -> s
  | Item { comp_unit; id } -> comp_unit ^ string_of_int id
  | Predef s -> s
  | _ -> "rien"

module Local_analysis = struct
  let expr poses expr =
    match expr with
    | { Typedtree.exp_desc = Texp_ident (id, _, _); exp_loc; _ } ->
        let extract_id id =
          match id with
          | Path.Pident id ->
              let uniq = Ident.unique_name id in
              poses := (Jmp_to_def uniq, exp_loc) :: !poses
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
        poses := (Def uniq, pat_loc) :: !poses
    | _ -> ()
end

module Global_analysis = struct
  let init poses uid_to_loc =
    Shape.Uid.Tbl.iter
      (fun uid t ->
        let s = string_of_uid uid in
        poses := (Def s, t) :: !poses)
      uid_to_loc
  let expr poses uid_to_loc expr =
    match expr with
    | { Typedtree.exp_desc = Texp_ident (_, _, value_description); exp_loc; _ }
      -> (
        match Shape.Uid.Tbl.find_opt uid_to_loc value_description.val_uid with
        | None -> ()
        | Some _ ->
            poses :=
              (Jmp_to_def (string_of_uid value_description.val_uid), exp_loc)
              :: !poses)
    | _ -> ()
end

let get_pos (cmt : Cmt_format.cmt_infos) =
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
