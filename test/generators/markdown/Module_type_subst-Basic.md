
# Module `Module_type_subst.Basic`

```ocaml
module type u = sig ... end
```
```ocaml
module type with_ = u with module type T = s
```
```ocaml
module type u2 = sig ... end
```
```ocaml
module type with_2 = u2 with module type T = sig ... end
```
```ocaml
module type a = sig ... end
```
```ocaml
module type c = a with module type b := s
```