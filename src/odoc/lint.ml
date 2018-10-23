open Compat

let mli file =
  let filename = Fs.File.to_string file in
  let items =
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf filename;
    try Parse.interface lexbuf with
    | Syntaxerr.Error e ->
      Format.printf "%a" Syntaxerr.report_error e;
      exit 1
  in
  let parent =
    let name = Filename.remove_extension filename in
    let file = Model.Root.Odoc_file.create_unit name ~force_hidden:false in
    let root = {Model.Root.package = ""; file; digest = Digest.file filename} in
    Model.Paths.Identifier.Root (root, name)
  in
  let mapper = {
    Ast_mapper.default_mapper with
    attribute = fun _self attr ->
      ignore (Loader.Attrs.read_comment parent attr);
      ignore (Loader.Attrs.read_attributes parent [attr]);
      attr
  } in
  ignore (mapper.signature mapper items)


let mld file =
  let filename = Fs.File.to_string file in
  let parent =
    let name = Filename.remove_extension filename in
    let file = Model.Root.Odoc_file.create_unit name ~force_hidden:false in
    let root = {Model.Root.package = ""; file; digest = Digest.file filename} in
    Model.Paths.Identifier.Root (root, name)
  in
  let location =
    let pos =
      Lexing.{
        pos_fname = filename;
        pos_lnum = 0;
        pos_cnum = 0;
        pos_bol = 0
      }
    in
    Location.{ loc_start = pos; loc_end = pos; loc_ghost = true }
  in
  match Fs.File.read file with
  | Ok content ->
    ignore (Loader.Attrs.read_string parent location content)
  | Error (`Msg err) ->
    prerr_endline err;
    exit 1
