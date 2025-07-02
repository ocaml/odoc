
# Module `Module_type_subst.Basic`

```
module type u = sig ... end
```
```
module type with_ = u with module type T = s
```
```
module type u2 = sig ... end
```
```
module type with_2 = u2 with module type T = sig ... end
```
```
module type a = sig ... end
```
```
module type c = a with module type b := s
```