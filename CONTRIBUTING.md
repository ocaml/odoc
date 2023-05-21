# Contributing

Welcome to the `odoc` project, and we're thrilled that you're considering contributing!

This guide serves as a roadmap for your contribution journey. It lays out the necessary steps and guidelines to ensure a smooth contribution experience, from setting up your development environment to making a pull request.

## Quick Start: HTML and CSS

The CSS for `odoc` is located at [src/html_support_files/odoc.css](https://github.com/ocaml/odoc/blob/master/src/html_support_files/odoc.css). We're continually working to improve it and welcome your contributions. You can edit CSS using your browser's [developer tools](https://developer.mozilla.org/en-US/docs/Tools) or [Chrome DevTools](https://developer.chrome.com/docs/devtools/). Once you've made your changes, submit a pull request with those changes.

Contributing to the HTML is slightly more complex. The main HTML generator is in [src/html/generator.ml](https://github.com/ocaml/odoc/blob/master/src/html/generator.ml). It operates on types defined in `Odoc_document.Types`, an intermediate representation used by all output renderers. The type describing an individual HTML page is `Odoc_document.Types.Page.t`.

Here's a step-by-step guide to get you set up for HTML contribution:

1. **Install requirements:**
   * Install a recent version of [HTML tidy](http://www.html-tidy.org/) (used for HTML validity testing). Here's how:
     - On MacOS (should be version 5.6.0 by the date of this writing): `brew install tidy-html5`   
     - On Debian / Ubuntu: `sudo apt-get install tidy`
   * Install a recent version of [jq](https://github.com/stedolan/jq). Here's how:
     - On MacOS: `brew install jq`
     - On Debian / Ubuntu: `sudo apt-get install jq`

2. **Set up for development:**
   Clone the odoc repository, navigate into the new directory, and install dependencies with these commands:
   ```
   git clone https://github.com/ocaml/odoc.git
   cd odoc
   opam pin add --no-action odoc .
   opam install --with-test --deps-only odoc
   ```

3. **Make and test changes:**
   After making your changes, compile the code with `make` and then run the tests with `make test`. Any changes to the HTML are likely to cause tests to fail - see the Testing section below to understand how to update them.

4. **Test `odoc` against your project:**
   Clean the build with `make clean`, then install `odoc` with `opam install odoc`. Because `odoc` is pinned, this will install your modified version. You can then run `odoc` in your project as you normally would: `dune build @doc`.

5. **Submit a pull request:**
   If everything looks good, feel free to submit a pull request. We appreciate your contribution!

## Testing

`odoc` uses a variety of different test types. We are slowly converging on using Dune's [cram tests](https://dune.readthedocs.io/en/stable/tests.html#cram-tests), though we still have many tests that aren't yet converted.

### Cram Tests

The tests extensively use these for the model layer and are found in [test/xref2](https://github.com/ocaml/odoc/blob/master/test/xref2). These consist of a directory called something.t, containing a file run.t. This file has shell-like syntax and usually runs `odoc` on some carefully crafted input files. 

For tests of the model layer, it's often useful to use the binary odoc_print which can dump .odoc and .odocl files as JSON. This output can then be piped through jq to verify that values are as expected.

We try to make these test files describe the test and what's expected, which helps when the output isn’t what the test expected. This also means that the tests can serve as documentation of how things work. As an example, see the file [test/xref2/multi_file_module_type_of.t/run.t](https://github.com/ocaml/odoc/blob/master/test/xref2/multi_file_module_type_of.t/run.t)

The tests work by executing the shell script snippets and then comparing the actual output with those in the run.t files. If these don't match, the difference is rendered as a diff.

If the test is broken, run `dune promote` to replace the expected output with the current output.

### Other Expect-Tests

Many of `odoc`'s older tests are custom Expect-tests, similar to those run in the Cram test above, but that don't use Dune's promote workflow.

When one of these Expect-tests fail, the output is saved, so the developer can choose to replace the now-incorrect expected string. For these custom Expect-tests, the results may look like the example given in the ocamldoc.

We are slowly shifting these custom Expect-tests over to the Dune promote workflow.

### Coverage Analysis

The `odoc` repo is set up for coverage analysis. This is most useful if you're writing new tests, and want to know what they’re actually touching. 

To use it:

- Run `make coverage`. This will run the tests as normal, except at the end you’ll get a message like
```
    See _coverage/index.html
```
You can then open `_coverage/index.html` and see the coverage of the code you’d like your new test to reach.

- Write new tests.
- Check coverage again.

### CI Tests

`odoc` is tested by [ocaml-ci](https://ci.ocamllabs.io/) and by GitHub workflows. One of these also does a coverage build, so we have up-to-date coverage stats on [Coveralls](https://coveralls.io/github/ocaml/odoc).

The tests cover Esy and Opam builds on Windows, macOS, and Linux. The Linux tests cover all supported versions of OCaml. We strive to retain compatibility back as far as we can (currently 4.02) which is important for supporting [ocaml.org/docs](https://ocaml.org/docs/).

## API Reference

### Loading

The [odoc.loader](https://ocaml.github.io/odoc/odoc_loader/index.html) library is responsible for converting from the OCaml [`Typedtree`](https://ocaml.github.io/odoc/deps/stdlib/Typedtree/index.html) representations to the [internal representation](https://ocaml.github.io/odoc/odoc_model/Odoc_model/Lang/index.html).

### Model

The [odoc.model](https://ocaml.github.io/odoc/odoc_model/index.html) library contains definitions of the internal types used to represent OCaml interfaces.

### Resolution and Expansion

Resolution of Paths, Fragments and References, and Expansion of Modules and Module Types are handled by the [odoc.xref2](https://ocaml.github.io/odoc/odoc_xref2/index.html) library.

### Intermediate Representation and Renderers

The generic documentation intermediate format is defined in the [odoc.document](https://ocaml.github.io/odoc/odoc_document/index.html) library.

The three current renderers are implemented within the following libraries: [odoc.html](https://ocaml.github.io/odoc/odoc_html/index.html), [odoc.latex](https://ocaml.github.io/odoc/odoc_latex/index.html), and [odoc.manpage](https://ocaml.github.io/odoc/odoc_manpage/index.html).

### CLI and Driver

The CLI for odoc and various helper functions for driving the process are contained in the [odoc.odoc](https://ocaml.github.io/odoc/odoc_odoc/index.html) library.

### Test and Internal Libraries

There are a couple of libraries used internally for testing - [odoc.xref_test](https://ocaml.github.io/odoc/odoc_xref_test/index.html) and [odoc.model_desc](https://ocaml.github.io/odoc/odoc_model_desc/index.html).

## Dependency Libraries

There are several [dependency libraries](https://ocaml.github.io/odoc/deps/index.html) that `odoc` uses, whose functions, type, and module declarations are essential for understanding how `odoc` works. See the [driver](https://ocaml.github.io/odoc/driver.html) page for details on how the documentation for these libraries are included.
