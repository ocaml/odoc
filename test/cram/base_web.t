  $ mkdir docs
Generating odocls for base with odig. This might give an error on some
dependencies so we do not display error (one was encountered with yojson)
  $ odig odoc --cache-dir=docs base 2> /dev/null
  Updating documentation, this may take some time...
  $ find ./docs/odoc/base/ -name '*.odocl' | sort
  ./docs/odoc/base/base.odocl
  ./docs/odoc/base/base__.odocl
  ./docs/odoc/base/base__Applicative.odocl
  ./docs/odoc/base/base__Applicative_intf.odocl
  ./docs/odoc/base/base__Array.odocl
  ./docs/odoc/base/base__Array0.odocl
  ./docs/odoc/base/base__Array_permute.odocl
  ./docs/odoc/base/base__Avltree.odocl
  ./docs/odoc/base/base__Backtrace.odocl
  ./docs/odoc/base/base__Binary_search.odocl
  ./docs/odoc/base/base__Binary_searchable.odocl
  ./docs/odoc/base/base__Binary_searchable_intf.odocl
  ./docs/odoc/base/base__Blit.odocl
  ./docs/odoc/base/base__Blit_intf.odocl
  ./docs/odoc/base/base__Bool.odocl
  ./docs/odoc/base/base__Bool0.odocl
  ./docs/odoc/base/base__Buffer.odocl
  ./docs/odoc/base/base__Buffer_intf.odocl
  ./docs/odoc/base/base__Bytes.odocl
  ./docs/odoc/base/base__Bytes0.odocl
  ./docs/odoc/base/base__Bytes_tr.odocl
  ./docs/odoc/base/base__Char.odocl
  ./docs/odoc/base/base__Char0.odocl
  ./docs/odoc/base/base__Comparable.odocl
  ./docs/odoc/base/base__Comparable_intf.odocl
  ./docs/odoc/base/base__Comparator.odocl
  ./docs/odoc/base/base__Comparisons.odocl
  ./docs/odoc/base/base__Container.odocl
  ./docs/odoc/base/base__Container_intf.odocl
  ./docs/odoc/base/base__Either.odocl
  ./docs/odoc/base/base__Either0.odocl
  ./docs/odoc/base/base__Either_intf.odocl
  ./docs/odoc/base/base__Equal.odocl
  ./docs/odoc/base/base__Error.odocl
  ./docs/odoc/base/base__Exn.odocl
  ./docs/odoc/base/base__Field.odocl
  ./docs/odoc/base/base__Fieldslib.odocl
  ./docs/odoc/base/base__Float.odocl
  ./docs/odoc/base/base__Float0.odocl
  ./docs/odoc/base/base__Floatable.odocl
  ./docs/odoc/base/base__Fn.odocl
  ./docs/odoc/base/base__Formatter.odocl
  ./docs/odoc/base/base__Globalize.odocl
  ./docs/odoc/base/base__Hash.odocl
  ./docs/odoc/base/base__Hash_intf.odocl
  ./docs/odoc/base/base__Hash_set.odocl
  ./docs/odoc/base/base__Hash_set_intf.odocl
  ./docs/odoc/base/base__Hashable.odocl
  ./docs/odoc/base/base__Hashable_intf.odocl
  ./docs/odoc/base/base__Hasher.odocl
  ./docs/odoc/base/base__Hashtbl.odocl
  ./docs/odoc/base/base__Hashtbl_intf.odocl
  ./docs/odoc/base/base__Hex_lexer.odocl
  ./docs/odoc/base/base__Identifiable.odocl
  ./docs/odoc/base/base__Identifiable_intf.odocl
  ./docs/odoc/base/base__Import.odocl
  ./docs/odoc/base/base__Import0.odocl
  ./docs/odoc/base/base__Indexed_container.odocl
  ./docs/odoc/base/base__Indexed_container_intf.odocl
  ./docs/odoc/base/base__Info.odocl
  ./docs/odoc/base/base__Info_intf.odocl
  ./docs/odoc/base/base__Int.odocl
  ./docs/odoc/base/base__Int0.odocl
  ./docs/odoc/base/base__Int32.odocl
  ./docs/odoc/base/base__Int63.odocl
  ./docs/odoc/base/base__Int63_emul.odocl
  ./docs/odoc/base/base__Int64.odocl
  ./docs/odoc/base/base__Int_conversions.odocl
  ./docs/odoc/base/base__Int_intf.odocl
  ./docs/odoc/base/base__Int_math.odocl
  ./docs/odoc/base/base__Intable.odocl
  ./docs/odoc/base/base__Invariant.odocl
  ./docs/odoc/base/base__Invariant_intf.odocl
  ./docs/odoc/base/base__Lazy.odocl
  ./docs/odoc/base/base__Linked_queue.odocl
  ./docs/odoc/base/base__Linked_queue0.odocl
  ./docs/odoc/base/base__List.odocl
  ./docs/odoc/base/base__List0.odocl
  ./docs/odoc/base/base__List1.odocl
  ./docs/odoc/base/base__Map.odocl
  ./docs/odoc/base/base__Map_intf.odocl
  ./docs/odoc/base/base__Maybe_bound.odocl
  ./docs/odoc/base/base__Monad.odocl
  ./docs/odoc/base/base__Monad_intf.odocl
  ./docs/odoc/base/base__Nativeint.odocl
  ./docs/odoc/base/base__Nothing.odocl
  ./docs/odoc/base/base__Obj_array.odocl
  ./docs/odoc/base/base__Obj_local.odocl
  ./docs/odoc/base/base__Option.odocl
  ./docs/odoc/base/base__Option_array.odocl
  ./docs/odoc/base/base__Or_error.odocl
  ./docs/odoc/base/base__Ordered_collection_common.odocl
  ./docs/odoc/base/base__Ordered_collection_common0.odocl
  ./docs/odoc/base/base__Ordering.odocl
  ./docs/odoc/base/base__Poly0.odocl
  ./docs/odoc/base/base__Popcount.odocl
  ./docs/odoc/base/base__Pow_overflow_bounds.odocl
  ./docs/odoc/base/base__Ppx_compare_lib.odocl
  ./docs/odoc/base/base__Ppx_enumerate_lib.odocl
  ./docs/odoc/base/base__Ppx_hash_lib.odocl
  ./docs/odoc/base/base__Pretty_printer.odocl
  ./docs/odoc/base/base__Printf.odocl
  ./docs/odoc/base/base__Queue.odocl
  ./docs/odoc/base/base__Queue_intf.odocl
  ./docs/odoc/base/base__Random.odocl
  ./docs/odoc/base/base__Random_repr.odocl
  ./docs/odoc/base/base__Ref.odocl
  ./docs/odoc/base/base__Result.odocl
  ./docs/odoc/base/base__Sequence.odocl
  ./docs/odoc/base/base__Set.odocl
  ./docs/odoc/base/base__Set_intf.odocl
  ./docs/odoc/base/base__Sexp.odocl
  ./docs/odoc/base/base__Sexp_with_comparable.odocl
  ./docs/odoc/base/base__Sexpable.odocl
  ./docs/odoc/base/base__Sign.odocl
  ./docs/odoc/base/base__Sign0.odocl
  ./docs/odoc/base/base__Sign_or_nan.odocl
  ./docs/odoc/base/base__Source_code_position.odocl
  ./docs/odoc/base/base__Source_code_position0.odocl
  ./docs/odoc/base/base__Stack.odocl
  ./docs/odoc/base/base__Stack_intf.odocl
  ./docs/odoc/base/base__Staged.odocl
  ./docs/odoc/base/base__String.odocl
  ./docs/odoc/base/base__String0.odocl
  ./docs/odoc/base/base__Stringable.odocl
  ./docs/odoc/base/base__Sys.odocl
  ./docs/odoc/base/base__Sys0.odocl
  ./docs/odoc/base/base__T.odocl
  ./docs/odoc/base/base__Type_equal.odocl
  ./docs/odoc/base/base__Uchar.odocl
  ./docs/odoc/base/base__Uchar0.odocl
  ./docs/odoc/base/base__Uniform_array.odocl
  ./docs/odoc/base/base__Unit.odocl
  ./docs/odoc/base/base__Variant.odocl
  ./docs/odoc/base/base__Variantslib.odocl
  ./docs/odoc/base/base__With_return.odocl
  ./docs/odoc/base/base__Word_size.odocl
  ./docs/odoc/base/base_internalhash_types/base_internalhash_types.odocl
  ./docs/odoc/base/caml/caml.odocl
  ./docs/odoc/base/md5/md5_lib.odocl
  ./docs/odoc/base/page-index.odocl
  ./docs/odoc/base/shadow_stdlib/shadow_stdlib.odocl
  $ cat $(find ./docs/odoc/base/ -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  6.2M	megaodocl
  $ sherlodoc index --index-docstring=true --index-name=true --type-search=true --format=js --db=db.js $(find ./docs/odoc/base/ -name '*.odocl') > /dev/null

  $ gzip -k db.js

  $ du -s *.js *.gz
  2108	db.js
  1592	db.js.gz

  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --search-uri=db.js --search-uri=sherlodoc.js --output-dir html $f
  > done
  $ odoc support-files -o html
  $ cp db.js html/
The --no-preserve flag is here so that copying to /tmp will not fail because of
a previous run. .js files built by dune are read only.
  $ cp --no-preserve=mode,ownership ../../jsoo/main.bc.js html/sherlodoc.js
  $ ls html
  base
  db.js
  fonts
  highlight.pack.js
  katex.min.css
  katex.min.js
  ocaml
  odoc.css
  odoc_search.js
  sexplib0
  sherlodoc.js
indent to see results
$ cp -r html /tmp
$ firefox /tmp/html/base/index.html
