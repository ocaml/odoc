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

let test cmi =
  match read_cmi cmi cmi with
  | Not_an_interface ->
      prerr_endline (cmi ^ ": not an interface");
      1
  | Wrong_version_interface ->
      prerr_endline (cmi ^ ": interface has wrong OCaml version");
      1
  | Corrupted_interface ->
      prerr_endline (cmi ^ ": corrupted interface");
      1
  | Not_a_typedtree ->
      prerr_endline (cmi ^ ": not a typedtree");
      1
  | Ok intf -> 0

let main () =
  let code = ref 0 in
  let test cmi =
    code := !code + (test cmi)
  in
    Arg.parse [] test "Test doc-ock on cmi files";
    exit !code

let () = main ()
