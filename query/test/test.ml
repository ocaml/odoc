open Query.Private

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
  (** This module does the same thing as Succ, but its correctness is obvious 
      and its performance terrible.  *)
  module Reference = struct
    include Set.Make (Int)

    let of_array arr = arr |> Array.to_seq |> of_seq
    let to_seq ~compare:_ = to_seq
  end

  (** This module is used to construct a pair of a "set array" using [Reference] 
      and a Succ that are exactly the same. *)
  module Both = struct
    let empty = Reference.empty, Succ.empty
    let union (l, l') (r, r') = Reference.union l r, Succ.union l' r'
    let inter (l, l') (r, r') = Reference.inter l r, Succ.inter l' r'
    let of_array arr = Reference.of_array arr, Succ.of_array arr
    let finish (arr, succ) = arr, Succ.finish succ
  end

  let extra_succ =
    Both.(
      finish
      @@ union
           (inter (of_array [| 0; 1 |]) (of_array [| 0; 1 |]))
           (inter (of_array [| 0; 2; 3 |]) (of_array [| 1; 3; 5; 7 |])))

  let rec random_set ~empty ~union ~inter ~of_array size =
    let random_set = random_set ~empty ~union ~inter ~of_array in
    if size = 0
    then empty
    else
      match Random.int 3 with
      | 0 ->
          let arr = Test_array.random_array size in
          Array.sort Int.compare arr ;
          of_array arr
      | 1 -> inter (random_set (size / 2)) (random_set (size / 2))
      | 2 -> union (random_set (size / 2)) (random_set (size / 2))
      | _ -> assert false

  let test_to_seq tree () =
    let ref =
      fst tree |> Reference.to_seq ~compare:Int.compare |> List.of_seq
    in
    let real = snd tree |> Succ.to_seq ~compare:Int.compare |> List.of_seq in
    Alcotest.(check (list int)) "same int list" ref real

  let tests_to_seq =
    [ Alcotest.test_case "Succ.to_seq extra" `Quick (test_to_seq extra_succ) ]
    @ List.init 50 (fun i ->
          let i = i * 7 in
          let succ =
            i |> Both.(random_set ~empty ~union ~inter ~of_array) |> Both.finish
          in
          Alcotest.test_case
            (Printf.sprintf "Succ.to_seq size %i" i)
            `Quick (test_to_seq succ))
end

let () =
  let open Alcotest in
  run "Query"
    [ "Array_succ", Test_array.tests_succ_ge @ Test_array.tests_succ_gt
    ; "Succ", Test_succ.tests_to_seq
    ]
