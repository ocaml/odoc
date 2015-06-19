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

open Assemblage

(* OCamlfind packages *)
let pkgs = [pkg "compiler-libs.common"; pkg "octavius"]

(* Doc flag *)
let doc = Flags.( v (`Compile `Intf) ["-doc"] )

(* No alias dependencies flag *)
let no_alias_deps = Flags.( v (`Compile `Byte) ["-no-alias-deps"]
                     @@@ v (`Compile `Native) ["-no-alias-deps"] )

(* Compilation units *)
let docOckPaths = unit "docOckPaths" (`Path ["src"])
let docOckTypes = unit "docOckTypes" (`Path ["src"])
let docOckMaps = unit "docOckMaps" (`Path ["src"])
let docOckPredef = unit "docOckPredef" (`Path ["src"])
let docOckNameEnv = unit "docOckNameEnv" (`Path ["src"])
let docOckIdentEnv = unit "docOckIdentEnv" (`Path ["src"])
let docOckLookup = unit "docOckLookup" (`Path ["src"])
let docOckAttrs = unit "docOckAttrs" (`Path ["src"])
let docOckCmi = unit "docOckCmi" (`Path ["src"])
let docOckCmti = unit "docOckCmti" (`Path ["src"])
let docOckCmt = unit "docOckCmt" (`Path ["src"])
let docOckComponents = unit "docOckComponents" (`Path ["src"])
let docOckComponentTbl = unit "docOckComponentTbl" (`Path ["src"])
let docOckResolve = unit "docOckResolve" (`Path ["src"])
let docOck = unit "docOck" ~flags:no_alias_deps (`Path ["src"])

let units =
  [ docOckPaths;
    docOckTypes;
    docOckMaps;
    docOckPredef;
    docOckNameEnv;
    docOckIdentEnv;
    docOckLookup;
    docOckAttrs;
    docOckCmi;
    docOckCmti;
    docOckCmt;
    docOckComponents;
    docOckComponentTbl;
    docOckResolve;
    docOck ]

(* Library *)
let l = lib (*~flags:doc*) ~deps:pkgs "doc-ock" (`Units units)

(* Assemble *)
let () = assemble (project "doc-ock-lib" [l])
