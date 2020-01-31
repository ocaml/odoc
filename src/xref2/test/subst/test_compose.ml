open Odoc_model
open Odoc_xref2

let module_ m = `LModule (Names.ModuleName.of_string m, 0)
let local_module m = `Local (module_ m)

let compare_sequential_and_composed ~s1 ~s2 test_case =
  let resolve = Subst.resolved_module_path in
  let sequential = resolve s2 (resolve s1 test_case)
  and composed = resolve (Subst.compose s1 s2) test_case in
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

let make_test_cases s =
  List.fold_left (fun acc (a, b) -> b :: a :: acc) [] s
  |> List.rev_map local_module

let test_subst ~s1 ~s2 =
  let pp_s fmt s = List.iter (fun (a, b) -> Format.fprintf fmt "%s%s" a b) s in
  Format.printf "Test %a . %a\n" pp_s s1 pp_s s2;
  let s1 = make_subst s1
  and s2 = make_subst s2
  and test_cases = make_test_cases s1 @ make_test_cases s2 in
  List.iter (compare_sequential_and_composed ~s1 ~s2) test_cases

let test () =
  test_subst ~s1:[ ("A", "B") ] ~s2:[ ("C", "D") ];
  test_subst ~s1:[ ("B", "C") ] ~s2:[ ("A", "B") ];
  test_subst ~s1:[ ("A", "B") ] ~s2:[ ("B", "C") ];
  ()
