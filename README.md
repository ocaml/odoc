# odoc

**odoc** is a new documentation generator for OCaml. It includes an accurate
cross-referencer, which handles the complexity of the OCaml module system.

odoc also offers a good opportunity to improve HTML output compared to ocamldoc,
but this is very much a work in progress :)

<br/>

## Installing and usage

```
opam install odoc
```

The easiest way to use odoc right now is by having Dune (Jbuilder) drive it.
This command should work in most Jbuilder projects out of the box:

```
jbuilder build @doc
```

The generated docs can then be found locally at
`./_build/default/_doc/_html/index.html`.

<br/>

## Contact

odoc is most discussed on [discuss.ocaml.org's Ecosystem category][discourse] with the `odoc` tag.
Please also don't hesitate to [open an issue][issues].

We may add a Gitter chat to the repo, or find some other way of communicating
that is more interactive :)

<br/>

## Contributing

Any [question asked](#contact), [issue opened][issues], feedback offered, is a
contribution to odoc, and the project and all its users are thankful :) If
you'd like to contribute code specifically, you may find the guide in
[`CONTRIBUTING.md`][contributing.md] helpful. If anything is missing from it,
please don't hesitate to [reach out](#contact) for help.

In the coming days, we will ask odoc users for a wishlist, and post a roadmap
based on it.

[discourse]: https://discuss.ocaml.org/c/eco
[issues]: https://github.com/ocaml/odoc/issues/new
[contributing.md]: https://github.com/ocaml/odoc/blob/master/CONTRIBUTING.md
