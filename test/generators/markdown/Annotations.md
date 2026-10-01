
# Module `Annotations`

```ocaml
type refcounted = {
  mutable readers : int [@atomic];
}
```