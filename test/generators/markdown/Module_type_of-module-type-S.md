
# Module type `Module_type_of.S`

```
module type T = sig ... end
```
```
module M : T
```
```
module N : module type of struct include M end
```