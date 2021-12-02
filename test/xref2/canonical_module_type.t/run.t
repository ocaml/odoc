In this test we're marking a ModuleType with a canonical path, then referencing
it later in the file. The resulting resolved path should contain a `CanonicalModuleType
constructor where the second element of the tuple is Resolved.

  $ cat test.mli
  (** @canonical Test.Y *)
  module type A = sig
    type t
  end
  
  module type B = sig
    (** The canonical tag is in the top-comment.
        @canonical Test.X *)
  
    type t
  end
  
  module type X = B
  
  module type Y = A
  
  module type Z = A

  $ ocamlc -c -bin-annot test.mli
  $ odoc compile --package x test.cmti
  $ odoc link test.odoc

Every module type aliases and the path they link to:

  $ odoc_print test.odocl | jq -c '.content.Module.items | .[] | select(.ModuleType.expr.Some.Path) | .ModuleType | { "from": .id, "to": .expr.Some.Path.p_path }'
  {"from":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"X"]},"to":{"`Resolved":{"`CanonicalModuleType":[{"`Identifier":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"B"]}},{"`Resolved":{"`Identifier":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"X"]}}}]}}}
  {"from":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"Y"]},"to":{"`Resolved":{"`CanonicalModuleType":[{"`Identifier":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"A"]}},{"`Resolved":{"`Identifier":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"Y"]}}}]}}}
  {"from":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"Z"]},"to":{"`Resolved":{"`CanonicalModuleType":[{"`Identifier":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"A"]}},{"`Resolved":{"`Identifier":{"`ModuleType":[{"`Root":[{"Some":{"`Page":["None","x"]}},"Test"]},"Y"]}}}]}}}

