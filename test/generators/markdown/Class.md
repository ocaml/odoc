
# Module `Class`

```ocaml
class type  empty = object ... end
```
```ocaml
class type  mutually = object ... end
```
```ocaml
class type  recursive = object ... end
```
```ocaml
class mutually' : mutually
```
```ocaml
class recursive' : recursive
```
```ocaml
class type virtual  empty_virtual = object ... end
```
```ocaml
class virtual empty_virtual' : empty
```
```ocaml
class type 'a polymorphic = object ... end
```
```ocaml
class 'a polymorphic' : 'a polymorphic
```