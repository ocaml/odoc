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

open DocOckPaths
open DocOckTypes

class ['a] signature ~equal (x : 'a Identifier.signature)
        (y : 'a Identifier.signature) = object

  inherit ['a] DocOckMaps.paths as super

  method root x = x

  method identifier_signature id =
    if Identifier.equal ~equal id x then y
    else super#identifier_signature id

  inherit ['a] DocOckMaps.types

end

let signature ~equal (x : 'a Identifier.signature)
      (y : 'a Identifier.signature) (s : 'a Signature.t) =
  (new signature ~equal x y)#signature s

class ['a] class_signature ~equal (x : 'a Identifier.class_signature)
        (y : 'a Identifier.class_signature) = object

  inherit ['a] DocOckMaps.paths as super

  method root x = x

  method identifier_class_signature id =
    if Identifier.equal ~equal id x then y
    else super#identifier_class_signature id

  inherit ['a] DocOckMaps.types

end

let class_signature ~equal (x : 'a Identifier.class_signature)
      (y : 'a Identifier.class_signature) (s : 'a ClassSignature.t) =
  (new class_signature ~equal x y)#class_signature s

class ['a] datatype ~equal (x : 'a Identifier.datatype)
        (y : 'a Identifier.datatype) = object

  inherit ['a] DocOckMaps.paths as super

  method root x = x

  method identifier_datatype id =
    if Identifier.equal ~equal id x then y
    else super#identifier_datatype id

  inherit ['a] DocOckMaps.types

end

let datatype ~equal (x : 'a Identifier.datatype)
      (y : 'a Identifier.datatype) (s : 'a TypeDecl.t) =
  (new datatype ~equal x y)#type_decl s
