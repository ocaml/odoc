```ocaml env=e1
open Odoc_model.Names
```

### Hidden paths

Tests for `weak_canonical_test`. When calling `is_resolved_module_hidden`, we normally would have `weak_canonical_test` false, and we'll check to see whether the canonical part of a path is hidden or not. When `weak_canonical_test` is true, we'll _assume_ that the canonical part will resolve to be a non-hidden path.

In the following test, we create a path with an unresolved canonical 
```ocaml env=e1
# let m = `Hidden (`Local (`LModule (ModuleName.hidden_of_string "M", 1)));;
val m : [> `Hidden of [> `Local of [> `LModule of ModuleName.t * int ] ] ] =
  `Hidden (`Local (`LModule (M, 1)))
# let cm = `Root (ModuleName.make_std "Foo");;
val cm : [> `Root of ModuleName.t ] = `Root Foo
# let p = `Canonical(m, cm);;
val p :
  [> `Canonical of
       [> `Hidden of [> `Local of [> `LModule of ModuleName.t * int ] ] ] *
       [> `Root of ModuleName.t ] ] =
  `Canonical (`Hidden (`Local (`LModule (M, 1))), `Root Foo)
```

At this point, `p` is a path to a hidden module that has an unresolved canonical constructor

In the following test, the when `weak_canonical_test` is true, the path should _not_ be hidden.

```ocaml env=e1
# let r1 = Cpath.is_resolved_module_hidden ~weak_canonical_test:false p;;
val r1 : bool = true
# let r2 = Cpath.is_resolved_module_hidden ~weak_canonical_test:true p;;
val r2 : bool = false
```

