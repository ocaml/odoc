An unboxed record read from a [.cmi] (no [-bin-annot]) keeps its unboxed
fields.

  $ cat > m.mli << EOF
  > type t = #{ x : int; y : float# }
  > EOF
  $ ocamlc -c m.mli
  $ odoc compile --package test m.cmi
  $ odoc link m.odoc
  $ odoc markdown-generate -o md m.odocl
  $ cat md/test/M.md
  
  # Module `M`
  
  ```ocaml
  type t = {
    x : int;
    y : float#;
  }
  ```
