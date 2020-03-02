open Odoc_model
open Odoc_xref2

let compare_sequential_and_composed ~resolve ~pp_path ~substs test_case =
  let sequential = List.fold_left (fun acc s -> resolve s acc) test_case substs
  and composed =
    let s = List.fold_left (Subst.compose) Subst.identity substs in
    resolve s test_case
  and delayed =
    let open Component.Substitution in
    let resolve_delayed = function DelayedSubst (s, p) -> resolve s p | NoSubst p -> p in
    let s = List.fold_left (Subst.Delayed.compose) (NoSubst test_case) substs in
    resolve_delayed s
  in
  if composed = sequential && sequential = delayed then
    Format.printf "\t%a -> %a\n" pp_path test_case pp_path composed
  else
    Format.printf "Error %a -> composed: %a != sequential: %a != delayed: %a\n"
      pp_path test_case pp_path composed pp_path sequential pp_path delayed

let test_all_subst ~make_local_path ~resolve ~pp_path ~subst_add substs =
  let pp_s fmt s = List.iter (fun (a, b) -> Format.fprintf fmt "%s%s" a b) s in
  let pp_ss fmt ss = List.iter (fun s -> Format.fprintf fmt " %a" pp_s s) ss in
  Format.printf "Test%a\n" pp_ss substs;
  let substs = List.map (List.fold_left subst_add Subst.identity) substs
  and test_cases =
    List.fold_left (List.fold_left (fun acc (a, b) -> b :: a :: acc)) [] substs
    |> List.sort_uniq compare |> List.map make_local_path
  in
  List.iter (compare_sequential_and_composed ~resolve ~pp_path ~substs) test_cases

let test_all_subst_module =
  let make_name m = `LModule (Names.ModuleName.of_string m, 0) in
  let make_local_path m = `Local (make_name m) in
  let resolve = Subst.resolved_module_path
  and pp_path = Component.Fmt.resolved_module_path
  and subst_add s (a, b) =
    let b = make_local_path b in
    Subst.add_module (make_name a) b s
  in
  test_all_subst ~make_local_path ~resolve ~pp_path ~subst_add

let test_type_expr substs_desc ~test_cases =
  let subst_add s (id, type_) = Subst.add_type_replacement id type_ s in
  let resolve = Subst.type_expr
  and pp_path = Component.Fmt.type_expr
  and substs = List.map (List.fold_left subst_add Subst.identity) substs_desc in
  let pp_s fmt (a, b) = Format.fprintf fmt "(%a -> %a)" Ident.fmt a pp_path b in
  let pp_ss fmt ss = List.iter (pp_s fmt) ss in
  let pp_sss fmt sss = List.iter (Format.fprintf fmt " %a" pp_ss) sss in
  Format.printf "Test%a\n" pp_sss substs_desc;
  List.iter (compare_sequential_and_composed ~resolve ~pp_path ~substs) test_cases

let test () =
  test_all_subst_module [ [ ("A", "B") ]; [ ("C", "D") ] ];
  test_all_subst_module [ [ ("B", "C") ]; [ ("A", "B") ] ];
  test_all_subst_module [ [ ("A", "B") ]; [ ("B", "C") ] ];
  test_all_subst_module [ [ ("A", "B") ]; [ ("C", "D") ]; [ ("B", "C") ] ];
  test_all_subst_module [ [ ("A", "B") ]; [ ("C", "D") ]; [ ("A", "C") ] ];
  test_all_subst_module [ [ ("A", "B"); ("C", "D") ]; [ ("D", "E"); ("F", "C") ]; [ ("B", "F") ] ];

  (* (Ident.path_type * TypeExpr.t) list *)
  (
    let type_ t = `LType (Names.TypeName.of_string t, 0) in
    let type_path t = `Resolved (`Local (type_ t)) in
    let open Component.TypeExpr in
    test_type_expr [
      [ type_ "a", Tuple [ Constr (type_path "b", []); Constr (type_path "c", []) ] ];
      [ type_ "c", Constr (type_path "d", []) ]
    ]
      ~test_cases:[ Constr (type_path "a", []) ]
  );
  ()
