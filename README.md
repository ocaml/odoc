# odoc &nbsp; [![Build Status][travis-img]][travis]

[travis]: https://travis-ci.org/ocaml/odoc/branches
[travis-img]: https://api.travis-ci.org/ocaml/odoc.svg?branch=master

**odoc** is a documentation generator for OCaml. It reads *doc comments* ,
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

## Using `odoc` with OCaml

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

## Using `odoc` with BuckleScript/Reason

> BuckleScript support currently requires the latest development version of odoc.

While the BuckleScript/Reason toolchain relies on `npm`, `odoc` at the moment
needs to be used from a working OCaml toolchain.

This means we follow the same installation than above, but using the
`4.02.3+buckle-master` version of the OCaml compiler.

```sh
λ opam switch 4.02.3+buckle-master
λ eval `opam config env`
λ opam pin add odoc.dev git+https://github.com/ocaml/odoc

```

Now with that working, we can point `odoc` to the path where BuckleScript saves
the compiled code that we can use to generate our documentation. This path is
`$root/lib/bs/src`.

In there you'll find your `.cmt` and `.cmti` files.

You can now compile each one of them from `.cmt[i]` to `.odoc` and from `.odoc`
to `.html`.

The following script can help you get started:

```bash
#!/bin/bash

readonly PKG=$1
readonly DOCS=$2

readonly ODOC=$(which odoc)
readonly LIB=./lib/bs/src

readonly CMT_FILES=$(find ${LIB} -name "*.cmti")
readonly ODOC_FILES=$(echo ${CMT_FILES} | sed "s/cmti/odoc/g")

echo "<< Compiling docs..."
for file in ${CMT_FILES}; do
  ${ODOC} compile \
    -I ${LIB} \
    --pkg=${PKG} \
    ${file}
done
echo ">> Done!"

echo "<< Generating HTML..."
for file in ${ODOC_FILES}; do
  ${ODOC} html \
    -I ${LIB} \
    -o ${DOCS} \
    --syntax=re \
    --semantic-uris \
    ${file}
done
echo ">> Done!"
```

And you can call it like:

```sh
λ ./mk-docs.sh MyPackageName ${path_to_docs_folder}
<< Compiling docs...
>> Done!
<< Generating HTML...
>> Done!
```

<br/>

## Contact

odoc is most discussed on [discuss.ocaml.org's Ecosystem category][discourse] with the `odoc` tag.
Please also don't hesitate to [open an issue][issues].

<br/>

## Contributing

Any [question asked](#contact), [issue opened][issues], feedback offered, is a
contribution to odoc, and the project and all its users are thankful :) If
you'd like to contribute code specifically, you may find the guide in
[`CONTRIBUTING.md`][contributing.md] helpful. Also see the [roadmap][roadmap]
there. If anything is missing from it, please don't hesitate to
[reach out](#contact).

[discourse]: https://discuss.ocaml.org/c/eco
[issues]: https://github.com/ocaml/odoc/issues/new
[contributing.md]: https://github.com/ocaml/odoc/blob/master/CONTRIBUTING.md#readme
[roadmap]: https://github.com/ocaml/odoc/blob/master/CONTRIBUTING.md#Roadmap
