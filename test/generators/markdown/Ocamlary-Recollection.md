
# Module `Ocamlary.Recollection`

This comment is for `CollectionModule`.


## Parameters

```ocaml
module C : COLLECTION
```

## Signature

```ocaml
type collection = C.element list
```
This comment is for `collection`.

```ocaml
type element = C.collection
```
```ocaml
module InnerModuleA : sig ... end
```
This comment is for `InnerModuleA`.

```ocaml
module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'
```
This comment is for `InnerModuleTypeA`.
