open Query.Private

let print_int ~ch i = output_string ch (string_of_int i)

module Test_array = struct
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
      |> List.sort_uniq Int.compare |> Array.of_list
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
end

open Query

module Test_succ = struct
  let rec mem ~compare t elt =
    let open Succ in
    match t with
    | All -> invalid_arg "Succ.succ_rec All"
    | Empty -> false
    | Array arr -> Array.exists (fun elt' -> compare elt elt' = 0) arr
    | Union (l, r) -> mem ~compare l elt || mem ~compare r elt
    | Inter (l, r) -> mem ~compare l elt && mem ~compare r elt

  let ( let* ) = Option.bind
  let ( let+ ) x f = Option.map f x

  let array_succ_reference ~strictness =
    let open Succ in
    match strictness with
    | Ge -> Test_array.succ_ge_reference
    | Gt -> Test_array.succ_gt_reference

  let rec succ_reference ~compare ~strictness t elt =
    (* Printf.printf "depth : %i\n" depth ; *)
    match t with
    | Succ.All -> invalid_arg "Succ.succ_rec All"
    | Empty -> None
    | Array arr -> array_succ_reference ~strictness ~compare elt arr
    | Union (l, r) ->
        let elt_r = succ_reference ~compare ~strictness r elt in
        let elt_l = succ_reference ~compare ~strictness l elt in
        Succ.best_opt ~compare elt_l elt_r
    | Inter (l, r) ->
        let rec loop elt =
          if mem ~compare r elt
          then Some elt
          else
            let* elt = succ_reference ~compare ~strictness:Gt l elt in
            loop elt
        in
        let* elt = succ_reference ~compare ~strictness l elt in
        loop elt

  let rec first_reference ~compare t =
    match t with
    | Succ.All -> invalid_arg "Succ.first All"
    | Empty -> None
    | Array s -> Some s.(0)
    | Inter (l, r) ->
        let rec loop elt =
          let* elt = elt in
          if mem ~compare r elt
          then Some elt
          else
            let elt = succ_reference ~strictness:Gt ~compare l elt in
            loop elt
        in
        loop (first_reference ~compare l)
    | Union (l, r) ->
        Succ.best_opt ~compare
          (first_reference ~compare l)
          (first_reference ~compare r)

  let _ = first_reference

  let rec random_succ size =
    if size = 0
    then Succ.empty
    else
      match Random.int 3 with
      | 0 ->
          let arr = Test_array.random_array size in
          Array.sort Int.compare arr ;
          Succ.of_array arr
      | 1 -> Succ.inter (random_succ (size / 2)) (random_succ (size / 2))
      | 2 -> Succ.union (random_succ (size / 2)) (random_succ (size / 2))
      | _ -> assert false

  let random_succ size =
    let t = random_succ size in
    t.s

  let tests_succ name test =
    List.init 20 (fun i ->
        let i = i * 5 in
        let elt = Random.full_int ((i * 2) + 1) in
        let succ = random_succ i in
        Alcotest.test_case
          (Printf.sprintf "%s size %i elt %i" name i elt)
          `Quick (test elt succ))

  let tests_succ_gt elt tree () =
    let strictness = Succ.Gt in
    Alcotest.(check (option int))
      "same int option"
      (succ_reference ~strictness ~compare:Int.compare tree elt)
      (Succ.succ ~strictness ~compare:Int.compare tree elt)

  let tests_succ_ge elt tree () =
    let strictness = Succ.Ge in
    let ref = succ_reference ~strictness ~compare:Int.compare tree elt in
    let real = Succ.succ ~strictness ~compare:Int.compare tree elt in
    Alcotest.(check (option int)) "same int option" ref real

  let tests_succ_ge = tests_succ "succ_ge" tests_succ_ge
  let tests_succ_gt = tests_succ "succ_gt" tests_succ_gt
end

let () =
  let open Alcotest in
  run "Query"
    [ "Array_succ", Test_array.tests_succ_ge @ Test_array.tests_succ_gt
    ; "Succ", Test_succ.tests_succ_ge @ Test_succ.tests_succ_gt
    ]
