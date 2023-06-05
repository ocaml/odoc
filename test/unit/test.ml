open Common

let rec succ_ge_reference i ~compare elt arr =
  Printf.printf "ref_succ_ge %i\n%!" i ;
  if i = Array.length arr
  then None
  else if ge ~compare arr.(i) elt
  then Some arr.(i)
  else succ_ge_reference (i + 1) ~compare elt arr

let rec succ_gt_reference i ~compare elt arr =
  if i = Array.length arr
  then None
  else if gt ~compare arr.(i) elt
  then Some arr.(i)
  else succ_gt_reference (i + 1) ~compare elt arr

let succ_ge_reference ~compare elt arr = succ_ge_reference 0 ~compare elt arr
let succ_gt_reference ~compare elt arr = succ_gt_reference 0 ~compare elt arr

let test_succ_ge elt arr () =
  Alcotest.(check (option int))
    "same int option"
    (succ_ge_reference ~compare:Int.compare elt arr)
    (Array.succ_ge ~compare:Int.compare elt arr)

let test_succ_gt elt arr () =
  Alcotest.(check (option int))
    "same int option"
    (succ_gt_reference ~compare:Int.compare elt arr)
    (Array.succ_gt ~compare:Int.compare elt arr)

let () = Random.init 123

(* The tests *)

let random_array size =
  let r =
    List.init size (fun _ -> Random.full_int (size * 2))
    |> List.sort_uniq Int.compare |> Array.of_list
  in

  r

let test_ge a b =
  Alcotest.test_case (Printf.sprintf "ge %i %i" a b) `Quick (fun () ->
      Alcotest.(check bool) "same bool" (ge ~compare:Int.compare a b) (a >= b))

let test_gt a b =
  Alcotest.test_case (Printf.sprintf "gt %i %i" a b) `Quick (fun () ->
      Alcotest.(check bool) "same bool" (gt ~compare:Int.compare a b) (a > b))

let test_lt a b =
  Alcotest.test_case (Printf.sprintf "lt %i %i" a b) `Quick (fun () ->
      Alcotest.(check bool) "same bool" (lt ~compare:Int.compare a b) (a < b))

let test_le a b =
  Alcotest.test_case (Printf.sprintf "le %i %i" a b) `Quick (fun () ->
      Alcotest.(check bool) "same bool" (le ~compare:Int.compare a b) (a <= b))

let test_operators =
  (let a = 12 and b = 12 in
   [ test_ge a b; test_gt a b; test_le a b; test_lt a b ])
  @ (let a = 12 and b = 14 in
     [ test_ge a b; test_gt a b; test_le a b; test_lt a b ])
  @
  let a = 15 and b = 10 in
  [ test_ge a b; test_gt a b; test_le a b; test_lt a b ]

let tests_arr name test =
  List.init 50 (fun i ->
      let elt = Random.full_int ((i * 2) + 1) in
      let arr = random_array i in
      let arr_string =
        if i <= 5
        then
          "[|"
          ^ (arr |> Array.to_list |> List.map string_of_int
           |> String.concat "; ")
          ^ "|]"
        else "[|...|]"
      in
      Alcotest.test_case
        (Printf.sprintf "%s %i %s " name elt arr_string)
        `Quick (test elt arr))

let tests_succ_ge = tests_arr "succ_ge" test_succ_ge
let tests_succ_gt = tests_arr "succ_gt" test_succ_gt

let () =
  let open Alcotest in
  run "Common"
    [ "Common", test_operators; "Array", tests_succ_ge @ tests_succ_gt ]
