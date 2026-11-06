(* Progress, as the build steps report it, for the progress display. *)

type phase = Compile | Link | Index | Generate

let phases = [ Compile; Link; Index; Generate ]

let label = function
  | Compile -> "Compiling"
  | Link -> "Linking"
  | Index -> "Indexing"
  | Generate -> "Generating"

let index = function Compile -> 0 | Link -> 1 | Index -> 2 | Generate -> 3
let totals = Array.init (List.length phases) (fun _ -> Atomic.make 0)
let dones = Array.init (List.length phases) (fun _ -> Atomic.make 0)
let expect phase n = ignore (Atomic.fetch_and_add totals.(index phase) n)
let did phase = Atomic.incr dones.(index phase)

(* What each worker is running. *)
let running = Atomic.make 0
let activity = ref [||]
let init_nprocs n = activity := Array.init n (fun _ -> Atomic.make "idle")

let worker_busy id description =
  Atomic.incr running;
  Atomic.set !activity.(id) description

let worker_idle id =
  Atomic.decr running;
  Atomic.set !activity.(id) "idle"

let finished = Atomic.make false
let finish () = Atomic.set finished true

let render_stats env nprocs =
  match Logs.level () with
  | Some (App | Warning) | None ->
      let open Progress in
      let clock = Eio.Stdenv.clock env in
      let bar label total =
        Line.(
          list [ lpad 16 (const label); bar total; rpad 10 (count_to total) ])
      in
      let phase_bars =
        List.map (fun p -> bar (label p) (Atomic.get totals.(index p))) phases
      in
      let config = Config.v ~persistent:false () in
      with_reporters ~config
        Multi.(
          lines phase_bars
          ++ line (bar "Processes" nprocs)
          ++ lines (List.init nprocs (fun _ -> Line.string)))
        (fun phase_reporters processes activities ->
          (* A bar is told how far it has moved, so remember where each one
             is. *)
          let shown = Array.map (fun _ -> 0) dones and shown_running = ref 0 in
          let rec loop () =
            Eio.Time.sleep clock 0.1;
            List.iteri
              (fun i report ->
                let n = Atomic.get dones.(i) in
                report (n - shown.(i));
                shown.(i) <- n)
              phase_reporters;
            let n = Atomic.get running in
            processes (n - !shown_running);
            shown_running := n;
            List.iteri
              (fun i report -> report (Atomic.get !activity.(i)))
              activities;
            if not (Atomic.get finished) then loop ()
          in
          loop ())
  | _ -> ()

(* Benchmark results. *)

(* The smallest, largest and mean of some values, and how many there are. *)
let summary = function
  | [] -> None
  | x :: _ as xs ->
      let n = List.length xs in
      Some
        ( List.fold_left min x xs,
          List.fold_left max x xs,
          List.fold_left ( +. ) 0. xs /. float n,
          n )

let metric ?units ?trend ~name ~description value =
  let opt key = function None -> [] | Some v -> [ (key, `String v) ] in
  `Assoc
    ([ ("name", `String name); ("value", value) ]
    @ opt "units" units
    @ [ ("description", `String description) ]
    @ opt "trend" trend)

let range f (min, max, avg) =
  `Assoc [ ("min", f min); ("max", f max); ("avg", f avg) ]

let lower = "lower-is-better"
let json_float x = `Float x
let json_int x = `Int (int_of_float x)
let times cmds = List.map (fun c -> c.Run.time) cmds

(* How often a command ran, and how long it took. *)
let compute_metric_cmd cmd =
  match summary (times (Run.filter_commands cmd)) with
  | None -> []
  | Some (min, max, avg, count) ->
      [
        metric ~name:("total-" ^ cmd) (`Int count)
          ~description:("Number of time 'odoc " ^ cmd ^ "' has run.");
        metric ~name:("time-" ^ cmd) ~units:"s" ~trend:lower
          (range json_float (min, max, avg))
          ~description:("Time taken by 'odoc " ^ cmd ^ "'");
      ]

(* How many files a command wrote, and how big they are. *)
let compute_sizes cmd sizes =
  let description = "files produced by 'odoc " ^ cmd ^ "'" in
  match summary sizes with
  | None -> []
  | Some (min, max, avg, count) ->
      [
        metric ~name:("produced-total-" ^ cmd) (`Int count)
          ~description:("Number of " ^ description);
        metric ~name:("produced-size-" ^ cmd) ~units:"b" ~trend:lower
          (range json_int (min, max, avg))
          ~description:("Size of " ^ description);
      ]

let file_size f =
  match Bos.OS.Path.stat f with
  | Ok st -> Some (float_of_int st.Unix.st_size)
  | Error _ -> None

(* The files a command wrote, one per run. *)
let compute_produced_cmd cmd =
  Run.filter_commands cmd
  |> List.filter_map (fun c -> Option.bind c.Run.output_file file_size)
  |> compute_sizes cmd

(* The files below [dir], for a command that writes a tree. *)
let compute_produced_tree cmd dir =
  Bos.OS.Dir.fold_contents ~dotfiles:true ~elements:`Files
    (fun f acc -> Option.to_list (file_size f) @ acc)
    [] dir
  |> Result.value ~default:[] |> compute_sizes cmd

(* The [k] slowest runs of a command. *)
let compute_longest_cmd cmd =
  let k = 5 in
  let slowest =
    times (Run.filter_commands cmd)
    |> List.sort (fun a b -> Float.compare b a)
    |> List.filteri (fun i _ -> i < k)
  in
  match summary slowest with
  | None -> []
  | Some (min, max, avg, _) ->
      [
        metric ~name:("longest-" ^ cmd) ~units:"s" ~trend:lower
          (range json_float (min, max, avg))
          ~description:
            (Printf.sprintf "Time taken by the %d longest calls to 'odoc %s'" k
               cmd);
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
