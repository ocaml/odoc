**Try it online at [doc.sherlocode.com](https://doc.sherlocode.com) !**

A rough prototype of a Hoogle-like search engine for OCaml documentation. It's full of bugs and todos, but works well enough for my purpose: Perhaps it will be useful to you too.
- The fuzzy type search is supported by a polarity search. As an example, the type `string -> int -> char` gets simplified to `{ -string, -int, +char }` which means that it consumes a `string` and an `int` and produces a `char` (irrespective of the order of the arguments). This yields good candidates which are then sorted by similarity with the query.
- The real magic is all the package documentation generated for [`ocaml.org/packages`](https://ocaml.org/packages), which I got my hands on thanks to insider trading (but don't have the bandwidth to share back... sorry!)

```
$ opam install --deps-only ./sherlodoc.opam
    # Note: your odoc version must match your odocl files

# To index all the odocl files in `/path/to/doc`:
$ dune exec -- ./index/index.exe /path/to/doc /path/to/result.db
    # `/path/to/doc` should contain a hierarchy of subfolders `libname/1.2.3/**/*.odocl`
    # `result.db` will be created or replaced

# To run the website:
$ dune exec -- ./www/www.exe /path/to/result.db
22.10.22 17:17:33.102                       Running at http://localhost:1234
```
