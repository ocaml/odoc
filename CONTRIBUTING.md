# Contributing to odoc

Please ask any questions you have about odoc, [open any issues][issues],
[offer feedback][contact], etc. All of these are valued contributions :)

If you'd like specifically to work on the code of odoc, we hope that you will
find the information in this file helpful.

[issues]: https://github.com/ocaml/odoc/issues
[contact]: https://github.com/ocaml/odoc#contact

<br/>

#### Table of contents

- [Quick start: HTML and CSS](#Quick_start)
- [Testing](#Testing)
  - [Debug prints](#Debug_prints)
  - [Expect tests](#Expect_tests)
  - [Coverage analysis](#Coverage_analysis)
- [Project structure](#Project_structure)

<br/>

<a id="Quick_start"></a>
## Quick start: HTML and CSS

The odoc CSS is found at [`src/odoc/etc/odoc.css`][css-file]. It needs a lot of
work, and PRs are very welcome. You can edit CSS using your browser's developer
mode, then send us a PR for the same changes made to this file.

Working on the HTML is more involved. The main HTML generator is in
[`src/html/to_html_tree.ml`][to-html-tree]. This has one function for each kind
of OCaml language item that needs to be displayed in docs.

To make edits to the HTML generation, run the following commands:

1. Set up for development:

    ```
    git clone https://github.com/ocaml/odoc.git
    cd odoc
    opam pin add --no-action odoc .
    opam install --deps-only odoc
    ```

2. Make changes to the code. To compile it,

    ```
    make
    ```

    To run the repo's tests,

    ```
    make test
    ```

    For smaller changes, you don't have to make the repo's tests pass. The
    change having the right effect on your use-case is more important.

    There could be a lot of failures due to how thorough the repo test suite is
    in places, and we can update the tests for you by pushing into your PR. For
    larger changes, see [Testing](#Testing) below.

3. To test odoc against your own project, install it

    ```
    make clean
    opam install odoc
    ```

   Since odoc is pinned, this installs your modified version. Then, you can run
   odoc in your project as normal:

    ```
    jbuilder build @doc
    ```

4. If all looks good, send odoc a PR :)

[css-file]: https://github.com/ocaml/odoc/blob/master/src/odoc/etc/odoc.css
[to-html-tree]: https://github.com/ocaml/odoc/blob/master/src/html/to_html_tree.ml

<br/>

<a id="Testing"></a>
## Testing

The basic testing instructions are covered in [Quick start](#Quick_start), but
here is some more detail on odoc's testing setup.

<br/>

<a id="Debug_prints"></a>
### Debug prints

If you want to display something during the execution of the tests, write to
STDERR with [`prerr_endline`][prerr_endline] or [`Printf.eprintf`][eprintf].
The [testing framework][alcotest] will display STDERR if a test fails.

[prerr_endline]: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#VALprerr_endline
[eprintf]: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Printf.html#VALeprintf
[alcotest]: https://github.com/mirage/alcotest

<br/>

<a id="Expect_tests"></a>
### Expect tests

Most of odoc's tests are *expect tests*, which means that they convert output
of some code that is being tested to strings, and then check that those strings
are correct:

1. The tests run some code, for example the odoc parser on the string `{e foo}`.
2. They take the output, in this case an AST representing "emphasized `foo`,"
   and convert that output to a string. In this case, it will be an S-expression
   roughly like `(emphasis (foo))`.
3. There is an *expected* copy of this S-expression in a file somewhere in the
   repo. If the S-expression from the code doesn't match the expected one, the
   test fails.

The reason for using expect tests is that when a test fails, there are two
possibilities:

1. The code being tested has become wrong, in which case the *first* failure
   should trigger fixing the code.
2. The code being tested has been changed in some way, but is correct (perhaps
   more correct than it used to be), and it is the test case that is wrong. It
   is possible that *dozens* or even *hundreds* of tests are now wrong. It is
   not practical to fix them fully by hand.

When an expect test fails, the string that the code emitted is saved, so that
the human developer can choose to *replace* the now-incorrect expected string.
In odoc, a test faiilure looks like this:

```
-- bold.000 [basic.] Failed --
in _build/_tests/bold.000.output:

{e foo}

--- expect/bold/basic.txt       2018-04-15 14:42:32.356895400 -0500
+++ _actual/bold/basic.txt      2018-04-15 17:36:26.812747400 -0500
@@ -2,5 +2,5 @@
   (ok
    (((f.ml (1 0) (1 7))
      (paragraph
-      (((f.ml (1 0) (1 7)) (bold (((f.ml (1 3) (1 6)) (word foo)))))))))))
+      (((f.ml (1 0) (1 7)) (emphasis (((f.ml (1 3) (1 6)) (word foo)))))))))))
  (warnings ()))

To replace expected output with actual, run

bash _build/default/test/parser/_actual/replace.sh
```

The intended response to this is:

1. Check the diff. If the `-` line is correct, the code is broken. If the `+`
   line is correct, the test is broken.
2. If the test is broken, copy/paste the command that the output suggests,
   and re-run the tests:

    ```
    bash _build/default/test/parser/_actual/replace.sh; make test
    ```

   This command is the same within one test category (e.g. HTML tests, parser
   tests), so if you have lots of tests to fix, you paste it once, then use
   UP, ENTER to repeat it over and over again, quickly checking each failure.

<br/>

<a id="Coverage_analysis"></a>
### Coverage analysis

The odoc repo is set up for coverage analysis. This is most useful if you're
writing new tests, and want to know what they are actually touching. To use it,

1. Run `make clean` once, before beginning to work with coverage. This rebuilds
   odoc with Bisect_ppx linked in.

2. Run `make coverage`. This will run the tests as normal, except at the end you
   will get a message like

    ```
    Coverage summary: 1914/2594 (73.79%)
    See _coverage/index.html
    ```

   You can then open `_coverage/index.html` and see the coverage of the code you
   would like your new test to reach. It is possible that it is already covered
   "accidentally" by tests that are checking other properties, however, in which
   case coverage analysis will not be very useful :)

3. Write new tests.

4. Check coverage again.

<br/>

<a id="Project_structure"></a>
## Project structure

The project is divided into several sub-libraries, each of which is a directory
under `src/`. Most of these have a *main file*, which has the same name as the
directory. That main file is the interface for the entire sub-library directory.
For example, [`src/parser`][parser-dir] has
[`src/parser/parser_.mli`][parser-api], and everything in `src/parser` is
hidden behind that interface.

The `jbuild` files in each directory can be used to figure out how the
directories depend on each other. Mostly, however, everything depends on
`model`, and `odoc` depends on everything.

The directories are:

- [`src/model`][model-dir] &mdash; datatypes representing the OCaml
language ([`src/model/lang.ml`][lang]), error-handling
([`src/model/error.ml`][error]), cross-references
([`src/model/paths-types.ml`][paths]), etc. This directory actually has no main
file :) It is a collection of the datatypes that the rest of the odoc
sub-libraries use to communicate with each other, so everything else depends on
`model`.

- [`src/loader`][loader-dir] &mdash; functions from `cmt`, `cmti`, `cmi` files
to `model`. You can see the three functions' signatures in the main file,
[`src/loader/loader.mli`][loader-api].

- [`src/parser`][parser-dir] &mdash; a single function from strings to comment
ASTs. You can see its signature in the main file,
[`src/parser/parser_.mli`][parser-api].

- [`src/xref`][xref-dir] &mdash; functions for resolving cross-references. These
consume things from `model`, and return transformed instances. The signature, in
[`src/xref/xref.mli`][xref-api] is not very pretty, but the usage of `xref` is
pretty isolated in the rest of odoc, and can be found by grepping for `Xref`.

- [`src/html`][html-dir] &mdash; the HTML generator. A neat main file for this
is still a [work in progress][html-api].

- [`src/odoc`][odoc-dir] &mdash; the overall `odoc` command-line tool that ties
the other parts together. This doesn't have the same kind of main file, because
what's generated from this is the odoc executable, not a sub-library. The entry
point for the executable is [`src/odoc/bin/main.ml`][main].

- [`src/util`][util-dir] is for things that help with the development of odoc,
but aren't part of the regular build, and [`src/vendor`][vendor-dir] is for
third-party software.

[model-dir]: https://github.com/ocaml/odoc/tree/master/src/model
[lang]: https://github.com/ocaml/odoc/blob/master/src/model/lang.ml
[error]: https://github.com/ocaml/odoc/blob/master/src/model/error.ml
[paths]: https://github.com/ocaml/odoc/blob/master/src/model/paths_types.ml
[parser-dir]: https://github.com/ocaml/odoc/tree/master/src/parser
[parser-api]: https://github.com/ocaml/odoc/blob/master/src/parser/parser_.mli
[loader-dir]: https://github.com/ocaml/odoc/tree/master/src/loader
[loader-api]: https://github.com/ocaml/odoc/blob/master/src/loader/loader.mli
[xref-dir]: https://github.com/ocaml/odoc/tree/master/src/xref
[xref-api]: https://github.com/ocaml/odoc/blob/master/src/xref/xref.mli
[html-dir]: https://github.com/ocaml/odoc/tree/master/src/html
[html-api]: https://github.com/ocaml/odoc/blob/master/src/html/html.ml
[odoc-dir]: https://github.com/ocaml/odoc/tree/master/src/odoc
[main]: https://github.com/ocaml/odoc/blob/master/src/odoc/bin/main.ml
[util-dir]: https://github.com/ocaml/odoc/tree/master/src/util
[vendor-dir]: https://github.com/ocaml/odoc/tree/master/src/vendor

The tests parallel the structure of `src/`:

- [`test/parser`][test-parser] is expect tests for the parser. Each [one]
[parser-test] calls the parser on a string, converts the AST to a string, and
compares it with an [expected string][parser-expect].

- [`test/html`][test-html] is end-to-end expect tests for the HTML generator.
Each [one][html-test] is an OCaml source file. The tester runs the `odoc` tool
on it, and compares the resulting HTML to some [expected HTML][html-expect].

- [`test/print`][test-print] is converters from odoc datatypes to strings, so
they can be used in expect tests.

- [`test/jbuilder`][test-jbuilder] is a tiny project for checking that Jbuilder
drives odoc correctly. It is mainly used in the odoc CI.

- [`test/inactive`][test-inactive] is some old tests that have suffered some bit
rot, and we haven't gotten around to restoring yet.

[test-parser]: https://github.com/ocaml/odoc/blob/master/test/parser/test.ml
[parser-test]: https://github.com/ocaml/odoc/blob/4c09575a5b25f4b224322f25d7867ce41fa4d032/test/parser/test.ml#L35
[parser-expect]: https://github.com/ocaml/odoc/blob/4c09575a5b25f4b224322f25d7867ce41fa4d032/test/parser/expect/one-paragraph/word.txt
[test-html]: https://github.com/ocaml/odoc/blob/master/test/html/test.ml
[html-test]: https://github.com/ocaml/odoc/blob/master/test/html/cases/val.mli
[html-expect]: https://github.com/ocaml/odoc/blob/master/test/html/expect/val.html
[test-print]: https://github.com/ocaml/odoc/tree/master/test/print
[test-jbuilder]: https://github.com/ocaml/odoc/tree/master/test/jbuilder
[test-inactive]: https://github.com/ocaml/odoc/tree/master/test/inactive