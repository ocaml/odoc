#if OCAML_VERSION >= (4, 14, 0)

type t = Shape.t -> Shape.Uid.t option

#if OCAML_VERSION >= (5, 2, 0)
let rec traverse_aliases = function
  | Shape_reduce.Resolved uid -> Some uid
  | Approximated id -> id
  | Resolved_alias (_, x) -> traverse_aliases x
  | _ -> None
#endif

let make ~lookup_impl =
  let read_unit_shape ~unit_name =
    match lookup_impl unit_name with
    | Some impl -> (
        match impl.Odoc_model.Lang.Implementation.shape_info with
        | Some (shape, _) -> Some shape
        | None -> None)
    | None -> None
  in
#if OCAML_VERSION < (5, 2, 0)
  let module Reduce = Shape.Make_reduce (struct
    type env = unit
    let fuel = 10
    let read_unit_shape ~unit_name = read_unit_shape ~unit_name
    let find_shape _ _ = raise Not_found
  end) in
  fun query ->
    match (try Some (Reduce.reduce () query) with Not_found -> None) with
    | Some result -> result.uid
    | None -> None
#else
  let module Reduce = Shape_reduce.Make (struct
    let fuel = 10
    let read_unit_shape ~unit_name = read_unit_shape ~unit_name
#if defined OXCAML
    let fuel () = Misc.Maybe_bounded.of_int 10
    let fuel_for_compilation_units () = Misc.Maybe_bounded.Unbounded
    let max_shape_reduce_steps_per_variable () = Misc.Maybe_bounded.Unbounded
    let max_compilation_unit_depth () = Misc.Maybe_bounded.Unbounded
    let projection_rules_for_merlin_enabled = true
    let read_unit_shape ~diagnostics:_ ~unit_name = read_unit_shape ~unit_name
#endif
  end) in
  fun query ->
    match
      (try Some (Reduce.reduce_for_uid Odoc_model.Paths.Ocaml_env.empty query)
       with Not_found -> None)
    with
    | Some r -> traverse_aliases r
    | None -> None
#endif

let reduce_for_uid t shape = t shape

#else

type t = unit

let make ~lookup_impl:_ = ()

#endif
