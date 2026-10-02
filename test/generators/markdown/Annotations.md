
# Module `Annotations`

```ocaml
type refcounted = {
  mutable readers : int [@atomic];
}
```
```ocaml
exception Atomic_exn of {
  mutable value : int [@atomic];
}
```
This exception has a record with an atomic annotation that should appear

```ocaml
type with_annot = 
  | Atomic_constr of {
    mutable value : int [@atomic];
  }
```
The annotation should appear on anonymous records in constructors as well
