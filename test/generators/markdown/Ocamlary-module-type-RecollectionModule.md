
# Module type `Ocamlary.RecollectionModule`

```
type collection = CollectionModule.element list
```
```
type element = CollectionModule.collection
```
```
module InnerModuleA : sig ... end
```
This comment is for `InnerModuleA`.

```
module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'
```
This comment is for `InnerModuleTypeA`.
