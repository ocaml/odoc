open Odoc
open DocOckHtml

let for_compile_step ~output input =
  let name =
    Fs.File.to_string input
    |> Filename.basename
    |> Filename.chop_extension
    |> fun s -> s ^ ".odoc"
  in
  [Fs.File.create ~directory:output ~name]

let unit ~env ~output:root_dir input =
  let unit = Unit.load input in
  let env = Env.build env unit in
  let odoctree = DocOck.resolve (Env.resolver env) unit in
  let odoctree = DocOck.expand (Env.expander env) odoctree in
  let root = Unit.root odoctree in
  let package = Root.(Package.to_string (package root)) in
  let targets = List_targets.unit ~package odoctree in
  (* CR-someday trefis: have [List_targets] return a tree instead of
     postprocessing. *)
  List.map targets ~f:(fun path ->
    let directory = Fs.Directory.of_string path in
    Fs.File.create ~directory ~name:"index.html"
  )

let index ~output:_ _ = []
