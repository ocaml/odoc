
# Module `Module_type_subst.Nested`

```
module type nested = sig ... end
```
```
module type with_ = nested with module type N.t = s
```
```
module type with_subst = nested with module type N.t := s
```