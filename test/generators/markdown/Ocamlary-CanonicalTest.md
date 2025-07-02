
# Module `Ocamlary.CanonicalTest`

```
module Base : sig ... end
```
```
module Base_Tests : sig ... end
```
```
module List_modif : module type of Base.List with type 'c t = 'c Base.List.t
```