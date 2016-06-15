open DocOck
open Odoc

let output_name input_name =
  let name = Filename.basename input_name in
  let extless =
    try Filename.chop_extension name
    with Invalid_argument _  -> name
  in
  extless ^ ".odoc"

let it's_all_the_same ~env ~output input reader =
  let input_name  = Fs.File.to_string input in
  let output_name = output_name input_name in
  let result = reader input_name in
  match result with
  | Not_an_interface  -> failwith "Not_an_interface"
  | Wrong_version  -> failwith "Wrong_version"
  | Corrupted  -> failwith "Corrupted"
  | Not_a_typedtree  -> failwith "Not_a_typedtree"
  | Not_an_implementation  -> failwith "Not_an_implementation"
  | Ok unit ->
    let env = Env.build env unit in
    let output = Fs.File.create ~directory:output ~name:output_name in
    resolve (Env.resolver env) unit
    |> expand (Env.expander env)
    |> Unit.save output

let root_of_unit ~package unit_name digest =
  let unit = Root.Unit.create unit_name in
  Root.create ~package ~unit ~digest


let cmti ~env ~output ~package input =
  it's_all_the_same ~env ~output input (read_cmti @@ root_of_unit ~package)

let cmt ~env ~output ~package input =
  it's_all_the_same ~env ~output input (read_cmt @@ root_of_unit ~package)

let cmi ~env ~output ~package input =
  it's_all_the_same ~env ~output input (read_cmi @@ root_of_unit ~package)
