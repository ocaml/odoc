
# Module `Module_type_subst.Nested`

```ocaml
module type nested = sig ... end
```
```ocaml
module type with_ = nested with module type N.t = s
```
```ocaml
module type with_subst = nested with module type N.t := s
```