open Odoc_xref2
open Odoc_xref_test


(* Simple strengthening *)
let name = "Simple strengthening"
let description = {|
This tests that strengthening works. When we strengthen a signature we recursively add
type equations for all abstract types.
|}

let input = {|

type t
type u = t

|}


let simple_strengthening () =
    let p = Common.root_identifier in
    let _, _, sg = Common.model_of_string input in
    let c = Component.Of_Lang.(signature empty sg) in
    let cp = Component.Of_Lang.(resolved_module_path empty p) in
    let c' = Strengthen.signature (`Resolved cp) c in
    let open Format in
    fprintf std_formatter "%s\n%s\n\n" name description;
    fprintf std_formatter "BEFORE\n======\n%!";
    fprintf std_formatter "%a\n%!" Component.Fmt.signature c;
    fprintf std_formatter "AFTER \n======\n%!";
    fprintf std_formatter "%a\n%!" Component.Fmt.signature c'

let _ =
    simple_strengthening ()