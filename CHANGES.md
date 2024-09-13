# 2.4.3

### Fixed

- Fix missing katex headers (@panglesd, #1096)
- Allow `][` in code blocks, fixing issue #1137 (@Julow, #1149)
  This was interpreted as "code blocks with result", which now mandates a
  delimiter: `{delim@lang[ code ]delim[ result ]}`
- Fix misprinting of type variables from ml files for OCaml 4.14 and later
  (multiple occurences of the same type variable could be named differently)
  (@octachron, #1173)

# 2.4.2

### Added

- OCaml 5.2.0 compatibility (@Octachron, #1094, #1112)

### Fixed

- Fix issues #1066 and #1095 with extended opens (@jonludlam, #1082, #1100)
 
# 2.4.1

### Fixed

- Revert to outputing a file (without content) when rendering a hidden
  compilation unit. This fixes cases where the dune rules would
  fail. (@panglesd, #1069)

# 2.4.0

### Added

- Add support for external search engines (@panglesd, @EmileTrotignon, #972)
  This includes the generation of an index and the display of the results in
  the UI (HTML only).

- Display 'private' keyword for private type extensions (@gpetiot, #1019)
- Allow to omit parent type in constructor reference (@panglesd,
  @EmileTrotignon, #933)

### Fixed

- Warn and exit when table(s) is not closed (@lubegasimon, #1050)
- Hint when list(s) is not closed (@lubegasimon, #1050)
- Fix crash on functors returning an alias (@Julow, #1046)
- Fix rendering of polymorphic variants (@wikku, @panglesd, #971)
- Add references to extension declarations (@gpetiot, @panglesd, #949)

### Changed

- Style: Adjusted line height in the TOC to improve readability (@sorawee, #1045)
- Style: Remove font fallback to Helvetica, Arial (@Julow, #1028)
- Style: Preformatted elements fallback to UA monospace (@toastal, #967)
- Style: Sidebar is now stuck to the left of the content instead of the left of
  the viewport (@EmileTrotignon, #999)

# 2.3.1

- Fix 5.1 support (@tmcgilchrist, #1018)

# 2.3.0

### Added
- Source code rendering (@Julow, @panglesd, @jonludlam #909, #996, #993, #982)
- Handle tables markup (@panglesd, @gpetiot, #893)
- Initial support for assets (@trefis, #975)
- odoc-parser remerged (@jonludlam, #973)
  This includes table support (@gpetiot, @panglesd, ocaml-doc/odoc-parser#11
  ocaml-doc/odoc-parser#14) and delimited code blocks with optional output
  (@jonludlam, ocaml-doc/odoc-parser#17)
- Add a tooltip to references with text (@Julow, #945)
- Add emoji to alerts in CSS (@yawaramin, #928)
- Add common language in shipped highlightjs (@Julow, #953)

### Fixed
- Fix `--hidden` not always taken into account (@panglesd, #940)
- Syntax highlight labels in function arguments (@panglesd, #990)
- Ensure generated html ends with a newline (@3Rafal, #954)
- Warn against tags in pages (@Julow, #948) 
- Remove unhelpful 'Unresolved_apply' errors (@gpetiot, #946)
- Allow links and references in headings (@EmileTrotignon, @panglesd, #942)
- Fix rendering of method types (@zoggy, #935)
- Fix section labelling with submodules (@EmileTrotignon, @panglesd, #931)
- LaTeX backend fixes (@Octachron, #921 #920)
- html: Remove extra space in class declarations (@Julow, #936)
- Fix rendering of unresolved references (@Julow, #957)

# 2.2.1

### Added
- OCaml 5.1.0 compatibility (@Octachron, #956)

# 2.2.0

### Added
- New unstable option `--as-json` for the HTML renderer that emits HTML
  fragments (preamble, content) together with metadata (table of contents,
  breadcrumbs, whether katex is used) in JSON format. (@sabine, #908)
- New maths support via `{m ... }` and `{math ... }` tags. (@giltho, @gpetiot, #886)
- Various optimisations (@jonludlam, #870, #883)
- Better handling of alerts and deprecation notices. (@panglesd, #828)
- Handle language tags on code blocks (@julow, #848)

### Fixed
- Shadowing issues (@jonludlam, #853)
- Layout fixes and improvements (@panglesd, #832, #839, #847) 
- Handle comments on class constraints and inherit (@julow, #844)
- Disable the missing root warning (@jonludlam, #881)

# 2.1.0

### Added
- New subcommand to resolve references (@panglesd, @lubegasimon, #812)
- Improved rendering of long signatures (@panglesd, #782)
- Handle comments attached to open statement as floating comment, instead
  of dropping them (@panglesd, #797)
- Empty includes (containing entirely shadowed entries) are now hidden (@panglesd, #798)

### Fixed
- Fix a missing Result constructor during compile. This will cause some
  functor arguments to have different filenames (@jonludlam, #795)
- Better memory/disk space usage when handling module alias chains (@jonludlam, #799)
- Resolving class-type paths (ie., `val x : #c`) (@jonludlam, #809)
- Skip top-level attributes while extracting the top comment. Fix top-comment extraction with PPX preprocessing (@jorisgio, #819)
- Better handling of @canonical tags (@jonludlam, #820)
- css: improved layout (@jonludlam, @Julow, #822)

# 2.0.2

### Added
- Compatibility with OCaml 4.14 (@patricoferris, @kit-ty-kate, #788)

# 2.0.1

### Fixed
- Man page renderer fails to output pages that have children (@jonludlam, @Julow, #766)
- Fix resolution of unprefixed references to pages (@Julow, #755)
- Fix reporting of ambiguous labels (@Julow, @jonludlam, #773, #781)
- Allow referencing of labels in the top comment (@jonludlam, #771)

### Added
- Strip unquoted spaces in identifiers for a more flexible reference syntax (@lubegasimon, @panglesd, #783)
- Add context to messages raised in expansions of includes (@Julow, #780)

# 2.0.0

### Changed
- Remove odoc-parser into a separate repository (@jonludlam, #700)

### Added
- OCaml 4.13 support (@octachron, #687, #689)
- Better errors/warnings (@Julow, #692, #717, #720, #732)
- ModuleType 'Alias' support (@jonludlam, #703)
- Improved test suite (@lubegasimon, #697)
- Improved documentation (@lubegasimon, @jonludlam, #702, #733)
- Strengthen module types (@jonludlam, #731)

### Fixed
- `uwt` now can be documented (@jonludlam, #708)
- Fix resolution involving deeply nested substitutions (@jonludlam, #727)
- Fix off-by-one error in error reporting (@asavahista, #736)


# 2.0.0~beta4

### Added
- Handle @canonical tags in the top-comment of modules (@Julow, #662)
- Simplify paths referring to Stdlib (@jonludlam, #677)
- New odoc command to report warnings encountered during compilation/linking (@Julow, #667)
- Anchors on type extensions (@Julow, #684)

### Fixed
- Resolve references in module synopses (@Julow, #658)
- Fix reference resolution in the presence of shadowing (@Julow, #682)

# 2.0.0~beta3

### Changed
- Refactor the comment parser in preparation for it to be octavius 2 (@jonludlam, #621)

### Added
- Better HTML rendering (@dbuenzli, #607, #612, #615)
- Better handling of signature comments (@Julow, #627, #629, #640, #643, #647, #654)
- Centre, left and right alignment constructs now parse correctly (@lubegasimon, #624)
- Allow reference to pages that contain hyphens and dots (@lubegasimon, #622)
- Allow type definitions to be copied and pasted with correct syntax (@Drup, #626)
- Install Ocamlary as a library for testing (@dbuenzli, #639)
- Handle @canonical tags on compilation units (@Julow, #649)
- Alias more when strengthening (@jonludlam, #653)
- Light theme fixes (@xvw, #660)

### Fixed
- LaTeX: Hardened description environments (@Octachron, #608)

# 2.0.0~beta2

### Added

- Add the ability to specifiy canonical paths for types and module types (@jonludlam, #596)
- Several improvements to the HTML tree (@dbuenzli, #600, #605, #589, @Drup, #579)
- Render module synopses in `{!modules:...}` (@Julow, #597)

### Fixed

- Fix for resolving references in mld files (@jonludlam, #611)
- Fix placement of documentation in module aliases (@Julow, #606)
- Fix breakage involving includes and shadowing (@jonludlam, #603)
- Don't link to hidden items (@lubegasimon, #583)
- Don't remove docs of inlined includes (@Julow, #595)
- Don't render shadowed values (@lubegasimon, #580)
- Fix unresolved references in the first comment of a file (@Julow, #592)

# 2.0.0~beta1

### Added

- New model for expanding and cross referencing (@jonludlam, @Julow, @lubegasimon)
- New document output layer, supporting HTML, LaTeX and man page output (@Drup, @Octachron, @jonludlam, @Julow, @lubegasimon)
- Experimental parent/child support for structured output (@jonludlam)

# 1.5.1

### Added

- Compatibility with OCaml 4.11 (#434, @kit-ty-kate)

# 1.5.0

### Added

- Add option to turn warnings into errors (#398, Jules Aguillon)

### Fixed

- Emit quote before identifier in alias type expr (Fixes #391, Anton Bachin)
- Handle generalized open statements introduced in 4.08 (#393, Jon Ludlam)
- Refactor error reporting to avoid exiting the program in library code
  (#400, Jules Aguillon)
- Build on OCaml 4.10 (#408, Jon Ludlam)

# 1.4.2

### Fixed

- Build on OCaml 4.09 (#383, Anil Madhavapeddy).
- Handle OCaml 4.08 type and module substitutions (#381, Jon Ludlam).
- Parser: better trimming of leading whitespace in code blocks (#370, Jules
  Aguillon).
- Parser: allow references to operators containing `:` characters (#384,
  reported Sylvain Le Gall).
- HTML: emit `<meta generator>` again (#378, Daniel Bünzli).
- CLI: `odoc html-targets` was ignoring deeply nested modules (#379, Daniel
  Bünzli).
- Fix bad internal usage of `List.iter2` (#376, Yotam Barnoy).

# 1.4.1

### Fixed

- Messy formatting in large definitions due to lack of `<span>`s (#360, Thomas
  Refis).
- Missing table of contents on `.mld` pages (#361, Rizo Isrof).
- Missing space before polymorphic class names (#339, Kevin Ji).
- Module type definitions printed with `:` instead of `=` (#344, Geoff Reedy).
- Conjunctive types printed with `*` instead of `&` (#353, Florian Angeletti).
- Type extensions (`+=`) printed without CSS classes found in other items (#348,
  reported Stéphane Lavergne).
- High memory usage on large codebases (#361, Thomas Refis).
- Build: double underscores in internal filenames (#357, Thomas Refis).
- Development: test suite assumed that html5-tidy supports `--mute` (#345,
  Geoff Reedy).
- Internal: refactored AST (#351, #364, Jules Aguillon).

# 1.4.0

### Changes

- All parsing errors are now recoverable warnings (#238).
- Page titles are now level-0 headings (`{0 ...}`), and top-level sections
  within a page are level-1 headings (`{1 ...}`) (#217, Rizo Isrof).
- Don't render definitions of externals (#275, Nik Graf).
- Disable programming ligatures (#248).
- Rename `--root-uri` option to `--xref-base-uri` (#223, Rizo Isrof).
- Deprecate redundant reference kind annotations (#246).

### Added

- Preliminary compatibility with the current 4.08 beta releases (#309, Jon
  Ludlam).
- Paragraph headings (`{4 ...}`) and subparagraph headings (`{5 ...}`) (#217,
  Rizo Isrof).
- `odoc support-files-targets` command (#232).
- Recommend [`bsdoc`](https://ostera.github.io/bsdoc/docs/BsDoc/) for using
  odoc with BuckleScript (#269, Leandro Ostera).

### Fixed

- Improve breadcrumbs on `.mld` pages (#293, Daniel Buenzli).
- Display tables of contents in nested module and class pages (#261, Rizo
  Isrof).
- Uncaught exception when parsing references to operators with `-` in them,
  such as `@->` (#178).
- Incorrect parsing of references to operators with `.` in them, such as `*.`
  (#237).
- Assertion failure when processing optional arguments in an `.ml` file with a
  type annotation, when that type annotation uses an alias of `'a option`
  (#101).
- Assertion failure when two modules with the same name are found by odoc (#148,
  Jon Ludlam).
- Verbatim blocks (`{v ... v}`) can now only be terminated if the `v}` is
  immediately preceded by whitespace (#71, reported Daniel Buenzli).
- Wrong column numbers for errors reported in comments (#227, #253).
- Restore parsing of ocamldoc-style reference kind annotations (#244).
- Ordinary `type` keyword instead of `and` rendered in HTML for
  mutually-recursive types (#105, reported @Fourchaux).
- `nonrec` keyword not rendered (#249).
- `and` not rendered for mutually-recursive modules, classes, and class types
  (#251).
- Outer comment attached to a module rendered when the module is included (#87,
  Jon Ludlam).
- Polymorphic variant constructor documentation not rendered (#176, reported
  @steinuil).
- Variant constructor and record field documentation styled differently (#260,
  Jon Ludlam).
- Sloppy keyword markup in HTML output (#319).
- Rendering of multiple `constraint` clauses (#321).
- Incorrect order of functor arguments (#261, Rizo Isrof).
- `odoc html` option `-o` now creates the output directory if it does not exist
  #171, #264 Rizo Isrof).
- `odoc html-targets` output now includes path prefix given through `-o` option
  (#173, Rizo Isrof).
- Allow `-I` and `-o` options to refer to non-existent directories (#32, #170,
  Daniel Buenzli).
- Make `odoc compile-targets` match `odoc compile` (#273, Daniel Buenzli).
- `odoc compile-deps` does not work on `.cmt` files (#162, Daniel Buenzli).
- `odoc html-deps` now scans for `.odoc` files recursively (#307, Daniel
  Buenzli).
- `odoc html-targets` ignores stop comments (#276, Daniel Buenzli).
- `odoc html-targets` and `odoc html-deps` segfault on `.mld` pages (#277, #282,
  Daneil Buenzli).
- `--theme-uri` option not propagated to some subpages (#318, Thomas Refis).
- Binary files not opened in binary mode (#281, Ulrik Strid).

### Internal

- Always print backtraces for unhandled exceptions (3d10feb).
- CI on macOS (#216, Leandro Ostera).
- Test runner improvements (#266, Rizo Isrof).
- Fix esy builds in Travis (#301, Jon Ludlam).
- Don't require `make` in the esy build (#308, Leandro Ostera).
- Get rid of some large GADTs (#292, Jon Ludlam).
- Remove dependency on `bos` (#305, Daniel Buenzli).
- Remove dependency on `rresult` (#306, Daniel Buenzli).
- Remove dependency on `bisect_ppx`, previously present in development
  checkouts (#316).

# 1.3.0

### Added

- Reason syntax output (#156, Patrick Stapfer).
- BuckleScript support (#179, Leandro Ostera).
- New CSS, appearance (#139, Rizo Isrof).
- Table of contents for the sections in each page (fe26392).
- Navigation breadcrumbs, and limit length of module paths in page
  titles (#150, Yotam Barnoy).
- Syntax highlighting of code blocks (99f2be9).
- Compiled `odoc` binary is now self-contained and requires no external
  files (bd3b53c).
- `--theme-uri` option (#154, Rizo Isrof).
- Option to convert `.mld` to HTML fragments rather than complete pages
  (#166, Rizo Isrof).

### Fixed

- Use regular dashes in arrows to support ligature fonts (#180, Leandro
  Ostera).
- Do not excessively indent code blocks (#133, Bobby Priambodo).
- Always prepend `page-` to output file name when compiling `.mld` files
  (#183, Rizo Isrof).
- Support `floatarray` type introduced in OCaml 4.06 (eb36158, Thomas
  Refis).
- Support destructive type substitution (57cbb4e, Thomas Refis).
- Render `<i>` tags in italics (#104, Thibault Suzanne).
- Flush HTML output correctly (#167, Rizo Isrof).
- Make HTML output more valid (#185, Leandro Ostera).
- Various improvements to parsing, output, documentation, and the
  development workflow (Yotam Barnoy, Luke Czyszczonik, Mohamed
  Elsharnouby, Rudi Grinberg, Rizo Isrof, Leandro Ostera, Bobby
  Priambodo, Thomas Refis, Patrick Stapfer).

### Internal

- `odoc` is now one repo.
- Dropped several dependencies.
- Considerable refactoring.
- New commnt parser (78a6699).
- Improved development workflow, including `CONTRIBUTING.md`, tests,
  coverage analysis, CI, and issue organization.
- Initial NPM packaging (Leandro Ostera, #214).
- Skeleton of odoc manual (Leandro Ostera, #203).

# 1.2.0

### Added

- Support for standalone documentation pages (`.mld` files) (#61).
- Display `[@@deprecated]` attributes as the `@deprecated` tag (#57).
- Allow each component of OCaml paths to be disambiguated using the `kind-identifer` syntax (part of #61).
- Support OCaml 4.06.

### Fixed
- Fix spurious leading blank lines in verbatim text (ocaml-doc/octavius#6).

# 1.1.1

### Changes

- make odoc more noisy when generating html for hidden units
- changed `html-deps` subcommand behavior: it now expects to be given a
  directory, not a single odoc file.

# 1.1.0

### Internal

- Switch to jbuilder

# 1.0.0

Initial release.
