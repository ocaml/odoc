**Try it online at [doc.sherlocode.com](https://doc.sherlocode.com) !**

A Hoogle-like search engine for OCaml documentation. It can be used in
differents ways, [online](https://doc.sherlocode.com), or offline with
the dev version of odoc.

It has fuzzy type search supported by a polarity search. As an example, the type
`string -> int -> char` gets simplified to `{ -string, -int, +char }` which
means that it consumes a `string` and an `int` and produces a `char`
(irrespective of the order of the arguments). This polarity search is fast
enough and yields good candidates which are then sorted by similarity with the
query. The sort is slower but the number of candidates is small.

You can search for anything that can exists in an MLI files : values, types,
modules, exceptions, constructors etc...

Fuzzy type search is available for values, sum-types constructors, exceptions,
and record fields.

# Usage

## Generating a search-database

The first step to using sherlodoc is generating a search-database. You do this
with the command `sherlodoc_index` :

```bash
sherlodoc_index --format=marshal -o db.bin a.odocl b.odocl
```

The `--format` option determines in which format the database is outputted. The
available format are `marshal`, `js`.  The `js` format, for
javascript, is the one compatible with odoc, and the `marshal` for most other
uses.

There is a third format : `ancient`, that is only available if the package
 `ancient` is installed. It is more complicated than the other two, you can read
on it [here](https://github.com/UnixJunkie/ocaml-ancient). It is used for the
[online](https://doc.sherlocode.com) version of sherlodoc, and is a mandatory
dependency of the `sherlodoc-www` package.

The `-o` option is the filename of the output.

Then you need to provide a list of .odocl files that contains the signatures
items that are going to be searchable. They are build artifacts of odoc.

There are others options that are documented by `sherlodoc_index --help`.

## Queries

To query sherlodoc, be it on the command-line or in a web interface, you need
to input a string query. A query is a list of words, separated by spaces.
Results will be entries that have every word of the list present in them.

```
"list map"
```

The above query will return entries that have both `list` and `map` in them.

You can also add `: <type>` at the end of your query, and in that case, results
will only be results whose type match <type>. This can only be a value, an
exception, a constructor or a record field.

Matching a type is fuzzy, if you do the following query :

```
"blabla : string"
```

It could return `val blablabla : int -> string` and `val blabla2 : string`.

You can have just the type-part of the query : `": string -> int"` is a valid
query.

You can use wildcards :

```
": string -> _"
```

will only return functions that take a string a argument, no matter what they
return.

There is limited support for polymorphism : you cannot search for `'a -> 'a` and
get every function `int -> int`, `string -> string` etc. However it will return
a function whose literal type is `'a -> 'a`. Having the first behaviour would
be a lot harder to program, and probably not a good idea, as it would be
impossible to search for polymorphic functions.

## Searching on the command line

If you have a search database in `marshal` format, you can search on the command
line :

```bash
sherlodoc --db=db.bin "blabla : int -> string"
```

`--db` is the filename of the search database. If absent, the environment
variable `SHERLODOC_DB` will be used instead.

In my example, I gave a query, but if you give none, sherlodoc enter an
interactive mode where you can enter queries until you decide to quit.

There are more option documented by `sherlodoc --help`, some of them are for
debugging/testing purposes, others might be useful.

### Search your switch

A reasonable use of sherlodoc on the cli is to search for signatures items from
your whole switch. Since odig can generate the documentation of the switch, we
can get the .odocl files with it :

Generate the documentation of your switch :

```bash
odig odoc
```

Generate the search database :

```bash
sherlodoc_index --format=marshal -o db.bin $(find $OPAM_SWITCH_PREFIX/var/cache/odig/odoc -name "*.odocl")
```

Enjoy searching :

```bash
sherlodoc --db-db.bin
```

## Searching from an odoc search bar

The latest unreleased version of odoc is compatible with sherlodoc. This allows
you to upload the documentation of a package with a search for this package
embedded.

For this to work, you need to generate a search database with format `js`, and
then add to every call of `odoc html-generate` the flags `--search-uri
sherlodoc.js --search-uri db.js`.

Be sure to copy the two js files in the output directory given to the
html-generate command :

```bash
cp $OPAM_SWITCH_PREFIX/share/sherlodoc/sherlodoc.js html_output/sherlodoc.js ;
cp db.js html_output/db.js ;
```

Obviously, most people use dune, and do not call `odoc html-generate`. A patch
for dune is being [worked on](https://github.com/emileTrotignon/dune/tree/search-odoc-new).
If you want to, you can test it, it should work. It is still work in progress.

## Sherlodoc online

If you want to use sherlodoc as a server, like on
[doc.sherlocode.com](https://doc.sherlocode.com), you can. This is packaged
separately in `sherlodoc-www`, but also lives in this repo.

Once you have installed `sherlodoc-www`, you need to generate your search
database :

```bash
sherlodoc_index --format=ancient --db=db.bin $(find /path/to/doc -name "*.odocl")
```

Then you can run the website :

```bash
sherlodoc-www db.bin
```

The real magic for [doc.sherlocode.com](https://doc.sherlocode.com) is all the
.odocl artifacts of the package documentation generated for
[`ocaml.org/packages`](https://ocaml.org/packages), which I got my hands on
thanks to insider trading (but don't have the bandwidth to share back... sorry!)
