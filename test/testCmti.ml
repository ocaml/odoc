(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open DocOck

class ident = object
  method root x = x
  method reference_value x = x
  method reference_type x = x
  method reference_module_type x = x
  method reference_module x = x
  method reference_method x = x
  method reference_label x = x
  method reference_instance_variable x = x
  method reference_field x = x
  method reference_extension x = x
  method reference_exception x = x
  method reference_constructor x = x
  method reference_class_type x = x
  method reference_class x = x
  method reference_any x = x
  method path_type x = x
  method path_module_type x = x
  method path_module x = x
  method path_class_type x = x
  method identifier_value x = x
  method identifier_type x = x
  method identifier_module_type x = x
  method identifier_module x = x
  method identifier_method x = x
  method identifier_label x = x
  method identifier_instance_variable x = x
  method identifier_field x = x
  method identifier_extension x = x
  method identifier_exception x = x
  method identifier_constructor x = x
  method identifier_class_type x = x
  method identifier_class x = x
  method fragment_type x = x
  method fragment_module x = x
  inherit [string] DocOckMaps.types
end

exception Bad_lookup
exception Bad_fetch of string

let test cmti =
  match read_cmti cmti cmti with
  | Not_an_interface ->
      prerr_endline (cmti ^ ": not an interface");
      1
  | Wrong_version_interface ->
      prerr_endline (cmti ^ ": interface has wrong OCaml version");
      1
  | Corrupted_interface ->
      prerr_endline (cmti ^ ": corrupted interface");
      1
  | Not_a_typedtree ->
      prerr_endline (cmti ^ ": not a typedtree");
      1
  | Ok intf ->
      let ident = new ident in
      let intf' = ident#unit intf in
        if intf != intf' then begin
          prerr_endline (cmti ^ ": deep identity map failed");
          1
        end else  begin
          let lookup u s =
            if u != intf' then raise Bad_lookup;
            if s = cmti then Some cmti
            else None
          in
          let fetch s =
            if s = cmti then intf'
            else raise (Bad_fetch s)
          in
          let resolver = build_resolver lookup fetch in
            try
              ignore (resolve resolver intf');
              0
            with
            | Bad_lookup ->
                prerr_endline (cmti ^ ": bad lookup during resolution");
                1
            | Bad_fetch s ->
                prerr_endline (cmti ^ ": bad fetch of " ^ s ^ " during resolution");
                1
          end


let main () =
  let code = ref 0 in
  let test cmti =
    code := !code + (test cmti)
  in
    Arg.parse [] test "Test doc-ock on cmti files";
    exit !code

let () = main ()
