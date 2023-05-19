Transparent ascription
======================

  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti

`Basic.P.N` should be a signature with just one type called `t`:

  $ odoc_print test.odoc -r Basic.P.N \
  > | jq '.type_.ModuleType.Signature.items
  >       | ((.[].Type | select(.)) |= { id: .[1].id."`Type"[1] })'
  [
    {
      "Type": {
        "id": "t"
      }
    }
  ]

`Nested.P1.N1.M` should be a module whose type is the resolved module type path
`T`:

  $ odoc_print test.odoc -r Nested.P1.N1.M \
  > | jq '.type_.ModuleType.Path.p_path."`Resolved"."`Identifier"."`ModuleType"[1]'
  "T"

`Nested.P1.N2` should be like `Basic.P.N`:

  $ odoc_print test.odoc -r Nested.P1.N2 \
  > | jq '.type_.ModuleType.Signature.items
  >       | ((.[].Type | select(.)) |= { id: .[1].id."`Type"[1] })'
  [
    {
      "Type": {
        "id": "t"
      }
    }
  ]

`Nested.P2.N1.M` should be like `Nested.P1.N1.M`:

  $ odoc_print test.odoc -r Nested.P2.N1.M \
  > | jq '.type_.ModuleType.Path.p_path."`Resolved"."`Identifier"."`ModuleType"[1]'
  "T"

`Nested.P2.N2` should be like `Basic.P.N`:

  $ odoc_print test.odoc -r Nested.P2.N2 \
  > | jq '.type_.ModuleType.Signature.items
  >       | ((.[].Type | select(.)) |= { id: .[1].id."`Type"[1] })'
  [
    {
      "Type": {
        "id": "t"
      }
    }
  ]

`Via_alias.P.N` should be like `Basic.P.N`:

  $ odoc_print test.odoc -r Via_alias.P.N \
  > | jq '.type_.ModuleType.Signature.items
  >       | ((.[].Type | select(.)) |= { id: .[1].id."`Type"[1] })'
  [
    {
      "Type": {
        "id": "t"
      }
    }
  ]

`Cascade.P.N1.I` should be like `Basic.P.N`:

  $ odoc_print test.odoc -r Cascade.P.N1.I \
  > | jq '.type_.ModuleType.Signature.items
  >       | ((.[].Type | select(.)) |= { id: .[1].id."`Type"[1] })'
  [
    {
      "Type": {
        "id": "t"
      }
    }
  ]

`Cascade.P.N2` should actually keep the module type `module type of
Cascade.P.O.I`, but with an expansion like `Basic.P.N`:

  $ odoc_print test.odoc -r Cascade.P.N2 \
  > | jq '.type_.ModuleType.TypeOf
  >       | (.t_desc.ModPath |= "..." + ."`Resolved"."`Module"[1])
  >       | (.t_expansion |=
  >            (.Some.Signature.items
  >             | ((.[].Type | select(.)) |= { id: .[1].id."`Type"[1] })))'
  {
    "t_desc": {
      "ModPath": "...I"
    },
    "t_expansion": [
      {
        "Type": {
          "id": "t"
        }
      }
    ]
  }

`In_functor_parameter.P.G` should be a functor type whose argument type has both
a type `t` *and* a value `plus`:

  $ odoc_print test.odoc -r In_functor_parameter.P.G \
  > | jq '.type_.ModuleType.Functor[0].Named.expr.Signature.items
  >       | ((.[].Type | select(.)) |= { id: .[1].id."`Type"[1] })
  >       | ((.[].Value | select(.)) |= { id: .id."`Value"[1] })'
  [
    {
      "Type": {
        "id": "t"
      }
    },
    {
      "Value": {
        "id": "plus"
      }
    }
  ]

Bug
===

  $ ocamlc -c -bin-annot bug.mli
  $ odoc compile bug.cmti

This should print the items in `Value` (i.e., just the value `print`). If the
bug from PR #958 is present, it will instead print the items in `Set`.

  $ odoc_print bug.odoc -r Inverse \
  > | jq '.type_.ModuleType.Functor[0].Named.expr.With.w_expansion.Some
  >       .Signature.items[]
  >       | select(.Module[1].id."`Module"[1] == "Point")
  >       | .Module[1].type_.ModuleType.Signature.items[].Include.expansion.content.items
  >       | ((.[].Value | select(.)) |= { id: .id."`Value"[1] })'
  [
    {
      "Value": {
        "id": "print"
      }
    }
  ]
