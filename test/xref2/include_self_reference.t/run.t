Test for include with type substitution referencing outer type.

When we have:
  $ cat test.mli
  (** Test case for include with type substitution referencing outer type.
  
      When the include's signature has a TypeSubstitution like [type reader := reader],
      the RHS [reader] refers to the outer type, not the substitution itself.
      This should not create a self-referential TypeSubstitution. *)
  
  module type Read = sig
    type reader = unit
  
    include sig
      type reader := reader
  
      val bin_read_unit : reader
    end
  end

The TypeSubstitution [type reader := reader] should substitute the inner
[reader] with the outer [reader] type. The RHS [reader] refers to the outer
type (type reader = unit), not the substitution itself.

Previously, items inside the include's signature expression were getting
identifiers with the same parent as siblings of the include statement.
This caused the TypeSubstitution's identifier to equal the outer type's
identifier, making the manifest appear as a self-reference.

  $ ocamlc -c -bin-annot test.mli

Compile and link should succeed without errors:

  $ odoc compile test.cmti
  $ odoc link test.odoc

The resulting signature should have:
- type reader = unit
- val bin_read_unit : reader (where reader refers to the type above)

Check that bin_read_unit has type reader (resolved to the outer type):

  $ odoc_print test.odocl -r Read.bin_read_unit | jq -c '.type_.Constr[0]'
  {"`Resolved":{"`Identifier":{"`Type":[{"`ModuleType":[{"`Root":["None","Test"]},"Read"]},"reader"]}}}

Generate HTML and verify the output is correct:

  $ odoc html-generate test.odocl -o html --indent

The module type Read should show both the type and the value:

  $ grep -E "type reader|val bin_read_unit" html/Test/module-type-Read/index.html | sed 's/<[^>]*>//g' | sed 's/^[[:space:]]*//'
