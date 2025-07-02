
# Module `Ocamlary.Recollection`

This comment is for `CollectionModule`.


## Parameters

```
module C : COLLECTION
```

## Signature

```
type collection = C.element list
```
This comment is for `collection`.

```
type element = C.collection
```
```
module InnerModuleA : sig ... end
```
This comment is for `InnerModuleA`.

```
module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'
```
This comment is for `InnerModuleTypeA`.
