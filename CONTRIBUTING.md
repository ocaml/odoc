# Contributing to odoc

Please ask any questions you have about odoc, [open any issues][issues],
[offer feedback][contact], etc. All of these are valued contributions :)

If you'd like specifically to work on the code of odoc, we hope that you will
find the information in this file helpful.

[contact]: https://github.com/ocaml/odoc#contact

<br/>

#### Table of contents

- [Quick start: HTML and CSS](#Quick_start)
- [Testing](#Testing)
  - [Debug prints](#Debug_prints)
  - [Coverage analysis](#Coverage_analysis)
- [Project structure](#Project_structure)
- [Roadmap](#Roadmap)
  - [Project status](#Project_status)
  - [General direction](#General_direction)
  - [Not supported in the near term](#Not_supported_in_the_near_term)
  - [Releases](#Releases)
  - [Issue organization](#Issue_organization)

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

1. Install requirements:

    - A recent version of [tidy](http://www.html-tidy.org/) (used for
      HTML validity testing) is required:

        ```
        # On MacOS (should be version 5.6.0 by the date of this writing)
        brew install tidy-html5
        
        # Debian / Ubuntu
        sudo apt-get install tidy
        ```

    - A recent version of [jq](https://github.com/stedolan/jq) is required.

        ```
        # On MacOS
        brew install jq
        
        # Debian / Ubuntu
        sudo apt-get install jq
        ```
2. Set up for development:

    ```
    git clone --recurse-submodules https://github.com/ocaml/odoc.git
    cd odoc
    opam pin add --no-action odoc .
    opam install --with-test --deps-only odoc
    opam install --deps-only mdx
    ```

    If you cloned the repository without the submodules, you can fetch
    them with: `git submodule update --init`

3. Make changes to the code. To compile it,

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

4. To test odoc against your own project, install it

    ```
    make clean
    opam install odoc
    ```

   Since odoc is pinned, this installs your modified version. Then, you can run
   odoc in your project as normal:

    ```
    dune build @doc
    ```

5. If all looks good, send odoc a PR :)

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

odoc is divided into several sub-libraries, each of which is a directory
under `src/`. Many of these have a *main file*, whose name is the directory
name prefixed with "`odoc_`". That main file is the interface for the entire
sub-library directory. For example, [`src/loader`][loader-dir] has
[`src/loader/odoc_loader.mli`][loader-api], and everything in `src/loader` is
hidden behind that interface.

The `dune` files in each directory can be used to figure out how the
directories depend on each other. Mostly, however, everything depends on
`model`, and `odoc` depends on everything.

The directories are:

- [`src/model`][model-dir] &mdash; datatypes representing the OCaml
language ([`src/model/lang.ml`][lang]), error-handling
([`src/model/error.ml`][error]), cross-references
([`src/model/paths_types.ml`][paths]), etc. This directory actually has no main
file. It is a collection of the datatypes that the rest of the odoc
sub-libraries use to communicate with each other, so everything else depends on
`model`.

- [`src/loader`][loader-dir] &mdash; functions from `cmt`, `cmti`, `cmi` files
to `model`.

- [`src/xref2`][xref2-dir] &mdash; functions for resolving cross-references. These
consume things from `model`, and return transformed instances.

- [`src/html`][html-dir] &mdash; the HTML generator.

- [`src/latex`][latex-dir] &mdash; the Latex generator.

- [`src/manpage`][manpage-dir] &mdash; the Manpages generator.

- [`src/odoc`][odoc-dir] &mdash; the `odoc` command-line interface (CLI) that ties the other
parts together. This doesn't have the same kind of main file, because what's
generated from this is the `odoc` executable, not a sub-library. The entry point
for the executable is [`src/odoc/bin/main.ml`][main].

- [`src/util`][util-dir] is for things that help with the development of odoc,
but aren't part of the regular build, and [`src/vendor`][vendor-dir] is for
third-party software.

[model-dir]: https://github.com/ocaml/odoc/tree/master/src/model
[lang]: https://github.com/ocaml/odoc/blob/master/src/model/lang.ml
[error]: https://github.com/ocaml/odoc/blob/master/src/model/error.ml
[paths]: https://github.com/ocaml/odoc/blob/master/src/model/paths_types.ml
[loader-dir]: https://github.com/ocaml/odoc/tree/master/src/loader
[loader-api]: https://github.com/ocaml/odoc/tree/master/src/loader/odoc_loader.ml
[xref2-dir]: https://github.com/ocaml/odoc/tree/master/src/xref2
[html-dir]: https://github.com/ocaml/odoc/tree/master/src/html
[latex-dir]: https://github.com/ocaml/odoc/tree/master/src/latex
[manpage-dir]: https://github.com/ocaml/odoc/tree/master/src/manpage
[odoc-dir]: https://github.com/ocaml/odoc/tree/master/src/odoc
[main]: https://github.com/ocaml/odoc/blob/master/src/odoc/bin/main.ml
[util-dir]: https://github.com/ocaml/odoc/tree/master/src/util
[vendor-dir]: https://github.com/ocaml/odoc/tree/master/src/vendor

The tests parallel the structure of `src/`:

- [`test/generators`][test-generators] &mdash; backend tests. See the [README.md][readme-md].

- [`test/inactive`][test-inactive] &mdash; old tests that have suffered some bit
rot, and we haven't gotten around to restoring yet.

- [`test/integration`][test-integration] &mdash; [cram tests][cram-tests] for the CLI and integration with dune.

- [`test/model`][test-model] &mdash; expect tests and cram tests for the odoc model.

- [`test/odoc_print`][test-odoc-print] &mdash; a helper program used by cram tests to print the content of intermediary files, that is `.odoc`, `.odocl` etc.

- [`test/pages`][test-pages] &mdash; tests for the CLI. It consists of shell commands and their expected output.

- [`test/xref2`][test-xref2] &mdash; mostly cram tests with some [mdx][mdx] tests for specific paths of compile and link code using the CLI and the "public" API (so they are end-to-end tests, not unit tests). [`xref2/compile.ml`][xref2-compile-ml] is a small script used to help write the cram tests.

[test-generators]: https://github.com/ocaml/odoc/tree/master/test/generators
[readme-md]: https://github.com/ocaml/odoc/blob/master/test/generators/README.md
[test-inactive]: https://github.com/ocaml/odoc/tree/master/test/inactive
[test-integration]: https://github.com/ocaml/odoc/tree/master/test/integration
[test-model]: https://github.com/ocaml/odoc/tree/master/test/model
[cram-tests]: https://dune.readthedocs.io/en/stable/tests.html?highlight=cram%20testing#cram-tests
[test-odoc-print]: https://github.com/ocaml/odoc/tree/master/test/odoc_print
[test-pages]: https://github.com/ocaml/odoc/tree/master/test/pages
[test-xref2]: https://github.com/ocaml/odoc/tree/master/test/xref2
[mdx]: https://github.com/realworldocaml/mdx
[xref2-compile-ml]: https://github.com/ocaml/odoc/blob/master/test/xref2/compile.ml


<br/>

<a id="Roadmap"></a>
## Roadmap

Everything here is subject to your input. Please discuss the roadmap in [#210, the roadmap issue][roadmap-issue].

[roadmap-issue]: https://github.com/ocaml/odoc/issues/210

<br/>

<a id="Project_status"></a>
### Project status

odoc is currently in **beta**. We aim for odoc to be good for diverse use cases
*in the future*, but for now we are focused on fast development satisfying
limited goals.

<br/>

<a id="General_direction"></a>
### General direction

The current goal of odoc is to become more useful for single projects. This
includes:

- **Quality of output** &mdash; Emitting good HTML, with usability features such
as whole-project search, etc. See the
[**Usability project**][usability-project].
- **Build integration** &mdash; Good interop with Dune for the OCaml and Reason
native ecosystems, and BuckleScript for the Reason/BuckleScript ecosystem. See
the [**Reason and BuckleScript project**][re-bs-project]. The Dune integration
is handled in the [Dune repo][dune].

Eventually, we want to start generating centralized docs for the entire OCaml
(and/or Reason) ecosystem, and hosting them at docs.ocaml.org. We are not
focused on this right now.

[usability-project]: https://github.com/ocaml/odoc/projects/1
[re-bs-project]: https://github.com/ocaml/odoc/projects/2
[dune]: https://github.com/ocaml/dune

<br/>

<a id="Not_supported_in_the_near_term"></a>
### Not supported in the near term

We'd like to support most of these things *eventually*, but the code base is
not ready for them, or we don't have enough time to implement them in the near
term. They are:

- The ability to emit HTML fragments.
- Compatibility with odig or other tools that drive odoc, besides the build
  systems Dune and bsb.
- Stable markup at the HTML level.
- Explicit custom themes.

<br/>

<a id="Releases"></a>
### Releases

We plan to release features fairly regularly (perhaps at most every 1-3 months).

odoc uses [**milestones**][milestones] for planned releases, with lists of
outstanding issues that they are to include. Note that many issues that have
already been resolved might not have been assigned to a milestone, but will
still be released.

If you'd like an issue to be added, please comment in it!

<br/>

<a id="Issue_organization"></a>
### Issue organization

- [**Milestones**][milestones] keep track of outstanding issues that definitely
  need to be done for a certain release.
- [**Projects**][projects] are long-term categories of issues. Visit each one,
  and you can see progress at a glance.
- We use several **labels** to give developers an idea of what each issue
  involves at a glance. See the [list of labels][labels], but they are really
  meant just to appear in the [issues list][issues] and be clickable.
- The [**good first issue**][easy-issues] label is meant to help new
  contributors find something they can get started with.

[milestones]: https://github.com/ocaml/odoc/milestones
[projects]: https://github.com/ocaml/odoc/projects
[labels]: https://github.com/ocaml/odoc/labels
[issues]: https://github.com/ocaml/odoc/issues
[easy-issues]: https://github.com/ocaml/odoc/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22
