# odoc

**odoc** is a documentation generator for OCaml. It reads *doc comments*,
delimited with `(** ... *)`, and outputs HTML. See example output at
[docs.mirage.io][mirage-docs].

[mirage-docs]: http://docs.mirage.io/

Text inside doc comments is marked up in ocamldoc syntax:

```ocaml
val compare : string -> string -> int
(** [compare s1 s2] compares [s1] and [s2] in {e lexicographic} order. The
    result is negative if [s1] precedes [s2], positive if [s1] follows [s2],
    and zero if [s1] and [s2] are equal. *)
```

The syntax reference is [here][comment-syntax]. There is also an
[explanation][comment-location] of how to attach comments to specific types,
values, and other elements in your program.

[comment-syntax]: http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#sec354
[comment-location]: http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#sec350

<br/>

odoc's main advantage over ocamldoc is an accurate cross-referencer, which
handles the complexity of the OCaml module system. odoc also offers a good
opportunity to improve HTML output compared to ocamldoc, but this is very much
a work in progress :)

<br/>

## Installing and usage

```
opam install odoc
```

The easiest way to use odoc right now is by having Dune drive it. This command
should work in most Dune projects out of the box:

```
dune build @doc
```

The generated docs can then be found locally at
`./_build/default/_doc/_html/index.html`.

<br/>

## Contact

odoc is most discussed on [discuss.ocaml.org's Ecosystem category][discourse] with the `odoc` tag.
Please also don't hesitate to [open an issue][issues].

<br/>

## Contributing

Any [question asked](#contact), [issue opened][issues], feedback offered, is a
contribution to odoc, and the project and all its users are thankful :) If
you'd like to contribute code specifically, you may find the guide in
[`CONTRIBUTING.md`][contributing.md] helpful. If anything is missing from it,
please don't hesitate to [reach out](#contact) for help.

[discourse]: https://discuss.ocaml.org/c/eco
[issues]: https://github.com/ocaml/odoc/issues/new
[contributing.md]: https://github.com/ocaml/odoc/blob/master/CONTRIBUTING.md
