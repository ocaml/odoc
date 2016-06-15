open DocOck
open Odoc

type t = Root.t Types.Unit.t

let save_xml file unit =
  let xml_folder = DocOckXmlFold.file Root.Xml.fold in
  let oc = open_out (Fs.File.to_string file) in
  let output = Xmlm.make_output ~nl:true ~indent:(Some 2) (`Channel oc) in
  xml_folder.DocOckXmlFold.f (fun () -> Xmlm.output output) () unit;
  close_out oc

let load_xml file =
  let xml_parser = DocOckXmlParse.build Root.Xml.parse in
  let ic = open_in (Fs.File.to_string file) in
  let input = Xmlm.make_input (`Channel ic) in
  let result = DocOckXmlParse.file xml_parser input in
  close_in ic;
  match result with
  | DocOckXmlParse.Ok unit -> unit
  | DocOckXmlParse.Error (_, (line, col), error_msg) ->
    let msg = Printf.sprintf "File %s, Line %d, column %d: %s"
                (Fs.File.to_string file) line col error_msg in
    failwith msg

let save file unit =
  let oc = open_out (Fs.File.to_string file) in
  Marshal.to_channel oc unit [];
  close_out oc

let load file =
  let ic = open_in (Fs.File.to_string file) in
  let res = Marshal.from_channel ic in
  close_in ic;
  res

let root (t : t) =
  match t.Types.Unit.id with
  | Paths.Identifier.Root (root, _) -> root
  | _ -> assert false
