```ocaml
let simple_strengthening input =
    let p = Common.root_identifier in
    let _, sg, _ = Common.model_of_string input in
    let c = Component.Of_Lang.(signature (empty ()) sg) in
    let cp = Component.Of_Lang.(resolved_module_path (empty ()) p) in
    let c' = Strengthen.signature (`Resolved cp) c in
    let open Format in
    fprintf std_formatter "BEFORE\n======\n%!";
    fprintf std_formatter "%a\n%!" Component.Fmt.signature c;
    fprintf std_formatter "AFTER \n======\n%!";
    fprintf std_formatter "%a\n%!" Component.Fmt.signature c'
```

Simple strengthening

This tests that strengthening works. When we strengthen a signature we recursively add
type equations for all abstract types.

```ocaml
# simple_strengthening {|
  type t
  type u = t
  |} ;;
BEFORE
======
type t/1
type u/0 = local(t/1,false)
 (removed=[])
AFTER
======
type t/2 = r((root Root)).t
type u/3 = local(t/2,false)
 (removed=[])
- : unit = ()
```
