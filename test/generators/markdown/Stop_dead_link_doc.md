
# Module `Stop_dead_link_doc`

```
module Foo : sig ... end
```
```
type foo = 
```
```
| Bar of Foo.t
```
```

```
```
type bar = 
```
```
| Bar of {
```
`field : Foo.t;`
```
}
```
```

```
```
type foo_ = 
```
```
| Bar_ of int * Foo.t * int
```
```

```
```
type bar_ = 
```
```
| Bar__ of Foo.t option
```
```

```
```
type another_foo = 
```
```
| Bar of Another_Foo.t
```
```

```
```
type another_bar = 
```
```
| Bar of {
```
`field : Another_Foo.t;`
```
}
```
```

```
```
type another_foo_ = 
```
```
| Bar_ of int * Another_Foo.t * int
```
```

```
```
type another_bar_ = 
```
```
| Bar__ of Another_Foo.t option
```
```

```