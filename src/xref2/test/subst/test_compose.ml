open Odoc_model
open Odoc_xref2

let compare_sequential_and_composed ~resolve ~pp_path ~substs test_case =
  let sequential = List.fold_left (fun acc s -> resolve s acc) test_case substs
  (* and composed = *)
  (*   let s = List.fold_left (Subst.compose) Subst.identity substs in *)
  (*   resolve s test_case *)
  and delayed =
    let open Component.Substitution in
    let resolve_delayed = function
      | DelayedSubst (s, p) -> resolve s p
      | NoSubst p -> p
    in
    let s = List.fold_left Subst.Delayed.compose (NoSubst test_case) substs in
    resolve_delayed s
  in
  let path = Alcotest.testable pp_path ( = ) in
  Alcotest.check path "same path" sequential delayed

let suite_name ~pp_id ~pp_path substs_desc =
  let open Fmt in
  let pp =
    pair ~sep:(unit " -> ") pp_id pp_path
    |> list ~sep:(unit "; ")
    |> parens
    |> list ~sep:(unit " . ")
  in
  to_to_string pp substs_desc |> String.map (function '/' -> '-' | x -> x)

(* Hack: Alcotest will make a file named like this *)

module All_subst = struct
  (** Build a [Subst.t] from a [(string * string) list list].
      Compare using composition vs applying the substitutions sequentially
      For all identifier *)

  let test_all_subst ~make_local_path ~resolve ~pp_path ~subst_add substs_desc =
    let substs = List.map (List.fold_left subst_add Subst.identity) substs_desc
    and test_cases =
      List.fold_left
        (List.fold_left (fun acc (a, b) -> b :: a :: acc))
        [] substs_desc
      |> List.sort_uniq compare
    in
    let make_test_case id =
      let test () =
        compare_sequential_and_composed ~resolve ~pp_path ~substs
          (make_local_path id)
      in
      Alcotest.test_case id `Quick test
    in
    ( suite_name ~pp_id:Fmt.string ~pp_path:Fmt.string substs_desc,
      List.map make_test_case test_cases )

  let test_module =
    let make_name m = `LModule (Names.ModuleName.of_string m, 0) in
    let make_local_path m = `Local (make_name m) in
    let resolve = Subst.resolved_module_path
    and pp_path = Component.Fmt.resolved_module_path
    and subst_add s (a, b) =
      let b = make_local_path b in
      Subst.add_module (make_name a) b s
    in
    test_all_subst ~make_local_path ~resolve ~pp_path ~subst_add
end

module Subst_type_expr = struct
  (** Test type replacement substitutions. *)

  include Component.TypeExpr

  let type_ t = `LType (Names.TypeName.of_string t, 0)

  let type_path t = `Resolved (`Local (type_ t))

  let constr t = Constr (type_path t, [])

  let test substs_desc ~test_cases =
    let subst_add s (id, type_) = Subst.add_type_replacement id type_ s in
    let resolve = Subst.type_expr
    and pp_path = Component.Fmt.type_expr
    and substs =
      List.map (List.fold_left subst_add Subst.identity) substs_desc
    in
    let make_test_case id =
      let test () =
        compare_sequential_and_composed ~resolve ~pp_path ~substs id
      in
      Alcotest.test_case (Fmt.to_to_string pp_path id) `Quick test
    in
    ( suite_name ~pp_id:Ident.fmt ~pp_path substs_desc,
      List.map make_test_case test_cases )
end

let () =
  Alcotest.run "Subst.compose"
    [
      All_subst.(test_module [ [ ("A", "B") ]; [ ("C", "D") ] ]);
      All_subst.(test_module [ [ ("B", "C") ]; [ ("A", "B") ] ]);
      All_subst.(test_module [ [ ("A", "B") ]; [ ("B", "C") ] ]);
      All_subst.(test_module [ [ ("A", "B") ]; [ ("C", "D") ]; [ ("B", "C") ] ]);
      All_subst.(test_module [ [ ("A", "B") ]; [ ("C", "D") ]; [ ("A", "C") ] ]);
      All_subst.(
        test_module
          [
            [ ("A", "B"); ("C", "D") ];
            [ ("D", "E"); ("F", "C") ];
            [ ("B", "F") ];
          ]);
      All_subst.(test_module [ [ ("A", "B") ]; [ ("A", "C") ] ]);
      Subst_type_expr.(
        test
          [
            [ (type_ "a", Tuple [ constr "b"; constr "c" ]) ];
            [ (type_ "c", constr "d") ];
          ]
          ~test_cases:[ constr "a" ]);
    ]
