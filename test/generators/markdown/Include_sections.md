
# Module `Include_sections`

```ocaml
module type Something = sig ... end
```
A module type.

Let's include [`Something`](./Include_sections-module-type-Something.md) once


## Something 1

foo


### Something 2


## Something 1-bis

Some text.


## Second include

Let's include [`Something`](./Include_sections-module-type-Something.md) a second time: the heading level should be shift here.


### Something 1

foo


#### Something 2


### Something 1-bis

Some text.


### Third include

Shifted some more.


#### Something 1

foo


##### Something 2


#### Something 1-bis

Some text.

And let's include it again, but without inlining it this time: the ToC shouldn't grow.

```ocaml
val something : unit
```

## Something 1

foo

```ocaml
val foo : unit
```

### Something 2

```ocaml
val bar : unit
```
foo bar


## Something 1-bis

Some text.
