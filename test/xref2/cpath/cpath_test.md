### Hidden paths

Tests for `weak_canonical_test`. When calling `is_resolved_module_hidden`, we normally would have `weak_canonical_test` false, and we'll check to see whether the canonical part of a path is hidden or not. When `weak_canonical_test` is true, we'll _assume_ that the canonical part will resolve to be a non-hidden path.

In the following test, we create a path with an unresolved canonical 
```ocaml env=e1
# let m = `Hidden (`Local (`LModule (Odoc_model.Names.ModuleName.internal_of_string "M", 1)));;
val m :
  [> `Hidden of
       [> `Local of [> `LModule of Odoc_model.Names.ModuleName.t * int ] ] ] =
  `Hidden (`Local (`LModule ({M}1, 1)))
# let cm = `Root "Foo";;
val cm : [> `Root of string ] = `Root "Foo"
# let p = `Canonical(m, cm);;
val p :
  [> `Canonical of
       [> `Hidden of
            [> `Local of [> `LModule of Odoc_model.Names.ModuleName.t * int ]
            ] ] *
       [> `Root of string ] ] =
  `Canonical (`Hidden (`Local (`LModule ({M}1, 1))), `Root "Foo")
# let r1 = Cpath.is_resolved_module_hidden ~weak_canonical_test:false p;;
val r1 : bool = true
# let r2 = Cpath.is_resolved_module_hidden ~weak_canonical_test:true p;;
val r2 : bool = false
```

