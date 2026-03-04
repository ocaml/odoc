
# Module type `Ocamlary.RecollectionModule`

```ocaml
type collection = CollectionModule.element list
```
```ocaml
type element = CollectionModule.collection
```
```ocaml
module InnerModuleA : sig ... end
```
This comment is for `InnerModuleA`.

```ocaml
module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'
```
This comment is for `InnerModuleTypeA`.
