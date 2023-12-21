open Query.Private

let rec succ_ge_reference i ~compare elt arr =
  Printf.printf "ref_succ_ge %i\n%!" i ;
  if i = Array.length arr
  then None
  else if compare arr.(i) elt >= 0
  then Some arr.(i)
  else succ_ge_reference (i + 1) ~compare elt arr

let rec succ_gt_reference i ~compare elt arr =
  if i = Array.length arr
  then None
  else if compare arr.(i) elt > 0
  then Some arr.(i)
  else succ_gt_reference (i + 1) ~compare elt arr

let succ_ge_reference ~compare elt arr = succ_ge_reference 0 ~compare elt arr
let succ_gt_reference ~compare elt arr = succ_gt_reference 0 ~compare elt arr

let test_succ_ge elt arr () =
  Alcotest.(check (option int))
    "same int option"
    (succ_ge_reference ~compare:Int.compare elt arr)
    (Array_succ.succ_ge ~compare:Int.compare elt arr)

let test_succ_gt elt arr () =
  Alcotest.(check (option int))
    "same int option"
    (succ_gt_reference ~compare:Int.compare elt arr)
    (Array_succ.succ_gt ~compare:Int.compare elt arr)

let () = Random.init 123

(* The tests *)

let random_array size =
  let r =
    List.init size (fun _ -> Random.full_int (size * 2))
    |> List.sort_uniq Int.compare
    |> Array.of_list
  in
  r

let tests_arr name test =
  List.init 50 (fun i ->
    let elt = Random.full_int ((i * 2) + 1) in
    let arr = random_array i in
    let arr_string =
      if i <= 5
      then
        "[|"
        ^ (arr |> Array.to_list |> List.map string_of_int |> String.concat "; ")
        ^ "|]"
      else "[|...|]"
    in
    Alcotest.test_case
      (Printf.sprintf "%s %i %s " name elt arr_string)
      `Quick
      (test elt arr))

let tests_succ_ge = tests_arr "succ_ge" test_succ_ge
let tests_succ_gt = tests_arr "succ_gt" test_succ_gt
