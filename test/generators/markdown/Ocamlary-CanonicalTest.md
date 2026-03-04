
# Module `Ocamlary.CanonicalTest`

```ocaml
module Base : sig ... end
```
```ocaml
module Base_Tests : sig ... end
```
```ocaml
module List_modif : module type of Base.List with type 'c t = 'c Base.List.t
```