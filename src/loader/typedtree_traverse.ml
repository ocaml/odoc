#if OCAML_VERSION >= (4, 14, 0)

module Analysis = struct
  type value_implementation = LocalValue of Ident.t | DefJmp of Shape.Uid.t

  type annotation = Definition of Ident.t | Value of value_implementation

  let expr uid_to_loc poses expr =
    let exp_loc = expr.Typedtree.exp_loc in
    if exp_loc.loc_ghost then ()
    else
      match expr.exp_desc with
      | Texp_ident (p, _, value_description) -> (
          let implementation =
            match
              Shape.Uid.Tbl.find_opt uid_to_loc value_description.val_uid
            with
            | Some _ -> Some (DefJmp value_description.val_uid)
            | None -> (
                match p with Pident id -> Some (LocalValue id) | _ -> None)
          in
          match implementation with
          | None -> ()
          | Some impl -> poses := (Value impl, exp_loc) :: !poses)
      | _ -> ()

  let pat env (type a) poses : a Typedtree.general_pattern -> unit = function
    | { Typedtree.pat_desc; pat_loc; _ } when not pat_loc.loc_ghost ->
        let maybe_localvalue id loc =
          match Ident_env.identifier_of_loc env loc with
          | None -> Some (Definition id, loc)
          | Some _ -> None
        in
        let () =
          match pat_desc with
#if OCAML_VERSION >= (5, 2, 0)
          | Tpat_var (id, loc, _uid) -> (
#else
          | Tpat_var (id, loc) -> (
#endif
              match maybe_localvalue id loc.loc with
              | Some x -> poses := x :: !poses
              | None -> ())
#if OCAML_VERSION >= (5, 2, 0)
          | Tpat_alias (_, id, loc, _uid) -> (
#else
          | Tpat_alias (_, id, loc) -> (
#endif
              match maybe_localvalue id loc.loc with
              | Some x -> poses := x :: !poses
              | None -> ())
          | _ -> ()
        in
        ()
    | _ -> ()
end

let of_cmt env uid_to_loc structure =
  let poses = ref [] in
  let expr iterator e =
    Analysis.expr uid_to_loc poses e;
    Tast_iterator.default_iterator.expr iterator e
  in
  let pat iterator e =
    Analysis.pat env poses e;
    Tast_iterator.default_iterator.pat iterator e
  in
  let iterator = { Tast_iterator.default_iterator with expr; pat } in
  iterator.structure iterator structure;
  !poses

#else

#endif
