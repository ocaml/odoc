(* Stats *)

(** Returns the [k] commands that took the most time for a given subcommand. *)

type stats = {
  mutable total_units : int Atomic.t;
  mutable total_impls : int Atomic.t;
  mutable total_mlds : int Atomic.t;
  mutable total_assets : int Atomic.t;
  mutable total_indexes : int Atomic.t;
  mutable non_hidden_units : int Atomic.t;
  mutable compiled_units : int Atomic.t;
  mutable compiled_impls : int Atomic.t;
  mutable compiled_mlds : int Atomic.t;
  mutable compiled_assets : int Atomic.t;
  mutable linked_units : int Atomic.t;
  mutable linked_impls : int Atomic.t;
  mutable linked_mlds : int Atomic.t;
  mutable generated_indexes : int Atomic.t;
  mutable generated_units : int Atomic.t;
  mutable processes : int Atomic.t;
  mutable process_activity : string Atomic.t Array.t;
  mutable finished : bool;
}

let stats =
  {
    total_units = Atomic.make 0;
    total_impls = Atomic.make 0;
    total_mlds = Atomic.make 0;
    total_assets = Atomic.make 0;
    total_indexes = Atomic.make 0;
    non_hidden_units = Atomic.make 0;
    compiled_units = Atomic.make 0;
    compiled_impls = Atomic.make 0;
    compiled_mlds = Atomic.make 0;
    compiled_assets = Atomic.make 0;
    linked_units = Atomic.make 0;
    linked_impls = Atomic.make 0;
    linked_mlds = Atomic.make 0;
    generated_units = Atomic.make 0;
    generated_indexes = Atomic.make 0;
    processes = Atomic.make 0;
    process_activity = [||];
    finished = false;
  }

let init_nprocs nprocs =
  stats.process_activity <- Array.init nprocs (fun _ -> Atomic.make "idle")

let pp_stats fmt stats =
  Fmt.pf fmt
    "Total units: %d\n\
     Total impls: %d\n\
     Total mlds: %d\n\
     Non-hidden units: %d\n\
     Compiled units: %d\n\
     Compiled impls: %d\n\
     Compiled mlds: %d\n\
     Linked units: %d\n\
     Linked impls: %d\n\
     Linked mlds: %d\n\
     Generated units: %d\n"
    (Atomic.get stats.total_units)
    (Atomic.get stats.total_impls)
    (Atomic.get stats.total_mlds)
    (Atomic.get stats.non_hidden_units)
    (Atomic.get stats.compiled_units)
    (Atomic.get stats.compiled_impls)
    (Atomic.get stats.compiled_mlds)
    (Atomic.get stats.linked_units)
    (Atomic.get stats.linked_impls)
    (Atomic.get stats.linked_mlds)
    (Atomic.get stats.generated_units)

let k_longest_commands cmd k =
  let open Run in
  filter_commands cmd
  |> List.sort (fun a b -> Float.compare b.time a.time)
  |> List.filteri (fun i _ -> i < k)

let dump () =
  let open Run in
  List.iter print_cmd (List.rev !commands);
  List.iter print_cmd (k_longest_commands "compile" 5);
  List.iter print_cmd (k_longest_commands "link" 5);
  List.iter print_cmd (k_longest_commands "html-generate" 5)

let rec compute_min_max_avg min_ max_ total count = function
  | [] -> (min_, max_, total /. float count, count)
  | hd :: tl ->
      compute_min_max_avg (min min_ hd) (max max_ hd) (total +. hd) (count + 1)
        tl

let compute_min_max_avg = function
  | [] -> None
  | hd :: tl -> Some (compute_min_max_avg hd hd hd 1 tl)

let compute_metric_int prefix suffix description values =
  match compute_min_max_avg values with
  | None -> []
  | Some (min, max, avg, count) ->
      let min = int_of_float min in
      let max = int_of_float max in
      let avg = int_of_float avg in
      [
        `Assoc
          [
            ("name", `String (prefix ^ "-total-" ^ suffix));
            ("value", `Int count);
            ("description", `String ("Number of " ^ description));
          ];
        `Assoc
          [
            ("name", `String (prefix ^ "-size-" ^ suffix));
            ( "value",
              `Assoc [ ("min", `Int min); ("max", `Int max); ("avg", `Int avg) ]
            );
            ("units", `String "b");
            ("description", `String ("Size of " ^ description));
            ("trend", `String "lower-is-better");
          ];
      ]

let compute_metric_cmd cmd =
  let open Run in
  let cmds = filter_commands cmd in
  let times = List.map (fun c -> c.Run.time) cmds in
  match compute_min_max_avg times with
  | None -> []
  | Some (min, max, avg, count) ->
      [
        `Assoc
          [
            ("name", `String ("total-" ^ cmd));
            ("value", `Int count);
            ( "description",
              `String ("Number of time 'odoc " ^ cmd ^ "' has run.") );
          ];
        `Assoc
          [
            ("name", `String ("time-" ^ cmd));
            ( "value",
              `Assoc
                [
                  ("min", `Float min); ("max", `Float max); ("avg", `Float avg);
                ] );
            ("units", `String "s");
            ("description", `String ("Time taken by 'odoc " ^ cmd ^ "'"));
            ("trend", `String "lower-is-better");
          ];
      ]

(** Analyze the size of files produced by a command. *)
let compute_produced_cmd cmd =
  let output_file_size c =
    match c.Run.output_file with
    | Some f -> (
        match Bos.OS.Path.stat f with
        | Ok st -> Some (float st.Unix.st_size)
        | Error _ -> None)
    | None -> None
  in
  let sizes = List.filter_map output_file_size (Run.filter_commands cmd) in
  compute_metric_int "produced" cmd
    ("files produced by 'odoc " ^ cmd ^ "'")
    sizes

(** Analyze the size of files outputed to the given directory. *)
let compute_produced_tree cmd dir =
  let acc_file_sizes path acc =
    match Bos.OS.Path.stat path with
    | Ok st -> float st.Unix.st_size :: acc
    | Error _ -> acc
  in
  Bos.OS.Dir.fold_contents ~dotfiles:true ~elements:`Files acc_file_sizes [] dir
  |> Result.get_ok
  |> compute_metric_int "produced" cmd ("files produced by 'odoc " ^ cmd ^ "'")

(** Analyze the running time of the slowest commands. *)
let compute_longest_cmd cmd =
  let k = 5 in
  let cmds = k_longest_commands cmd k in
  let times = List.map (fun c -> c.Run.time) cmds in
  match compute_min_max_avg times with
  | None -> []
  | Some (min, max, avg, _count) ->
      [
        `Assoc
          [
            ("name", `String ("longest-" ^ cmd));
            ( "value",
              `Assoc
                [
                  ("min", `Float min); ("max", `Float max); ("avg", `Float avg);
                ] );
            ("units", `String "s");
            ( "description",
              `String
                (Printf.sprintf
                   "Time taken by the %d longest calls to 'odoc %s'" k cmd) );
            ("trend", `String "lower-is-better");
          ];
      ]

let all_metrics html_dir =
  compute_metric_cmd "compile"
  @ compute_metric_cmd "compile-deps"
  @ compute_metric_cmd "link"
  @ compute_metric_cmd "html-generate"
  @ compute_longest_cmd "compile"
  @ compute_longest_cmd "link"
  @ compute_produced_cmd "compile"
  @ compute_produced_cmd "link"
  @ compute_produced_tree "html-generate" html_dir

let bench_results html_dir =
  let result =
    `Assoc
      [
        ("name", `String "odoc");
        ( "results",
          `List
            [
              `Assoc
                [
                  ("name", `String "driver.mld");
                  ("metrics", `List (all_metrics html_dir));
                ];
            ] );
      ]
  in
  Yojson.to_file "driver-benchmarks.json" result

let total_time () =
  let open Run in
  let cmds = !commands in
  List.fold_left (fun acc c -> acc +. c.time) 0.0 cmds
