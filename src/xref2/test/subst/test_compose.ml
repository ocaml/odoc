open Odoc_model
open Odoc_xref2

let module_ m = `LModule (Names.ModuleName.of_string m, 0)
let local_module m = `Local (module_ m)

let compare_sequential_and_composed ~substs test_case =
  let resolve = Subst.resolved_module_path in
  let sequential = List.fold_left (fun acc s -> resolve s acc) test_case substs
  and composed =
    let s = List.fold_left (Subst.compose) Subst.identity substs in
    resolve s test_case
  in
  let pp_p = Component.Fmt.resolved_module_path in
  if composed = sequential then
    Format.printf "\t%a -> %a\n" pp_p test_case pp_p composed
  else
    Format.printf "Error %a -> composed: %a != sequential: %a\n"
      pp_p test_case pp_p composed pp_p sequential

let make_subst =
  let f s (a, b) =
    let b = local_module b in
    Subst.add_module (module_ a) b s
  in
  List.fold_left f Subst.identity

let test_subst substs =
  let pp_s fmt s = List.iter (fun (a, b) -> Format.fprintf fmt "%s%s" a b) s in
  let pp_ss fmt ss = List.iter (fun s -> Format.fprintf fmt " %a" pp_s s) ss in
  Format.printf "Test%a\n" pp_ss substs;
  let substs = List.map make_subst substs
  and test_cases =
    List.fold_left (List.fold_left (fun acc (a, b) -> b :: a :: acc)) [] substs
    |> List.sort_uniq compare |> List.map local_module
  in
  List.iter (compare_sequential_and_composed ~substs) test_cases

let test () =
  test_subst [ [ ("A", "B") ]; [ ("C", "D") ] ];
  test_subst [ [ ("B", "C") ]; [ ("A", "B") ] ];
  test_subst [ [ ("A", "B") ]; [ ("B", "C") ] ];
  test_subst [ [ ("A", "B") ]; [ ("C", "D") ]; [ ("B", "C") ] ];
  test_subst [ [ ("A", "B") ]; [ ("C", "D") ]; [ ("A", "C") ] ];
  test_subst [ [ ("A", "B"); ("C", "D") ]; [ ("D", "E"); ("F", "C") ]; [ ("B", "F") ] ];
  ()
