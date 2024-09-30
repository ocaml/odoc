Unfortunately, cram test do not work well to test `odoc_driver`, due to
interaction with opam and the sandboxing.

Here is how to test the "pkg_args" feature, **by hand**.

From the test_odoc_driver/ folder:

```shell
$ opam pin .
```

Have a look at the `test_odoc_driver/odoc-config.sexp` file to see how it looks

```shell
$ cat test_odoc_driver/odoc-config.sexp
(packages brr)
(libraries brr brr.poke)
```

From where you want (Most likely the root of the repo):

```shell
$ dune exec -- odoc_driver -p test_odoc_driver -p brr --link-grep page-yo
'odoc' 'link' '_odoc/test_odoc_driver/doc/page-yo.odoc' '-o' '_odoc/test_odoc_driver/doc/page-yo.odocl' '-I' '_odoc/test_odoc_driver/doc/' '-I' 'test_odoc_driver/lib/test_odoc_driver' '-P' 'brr:_odoc/brr/doc' '-P' 'test_odoc_driver:_odoc/test_odoc_driver/doc' '-L' 'brr.poke:_odoc/brr/lib/brr.poke' '-L' 'brr:_odoc/brr/lib/brr' '-L' 'test_odoc_driver:_odoc/test_odoc_driver/lib/test_odoc_driver' '--current-package' 'test_odoc_driver' '--enable-missing-root-warning'
```

You can also check the references in the `yo` page resolves, by visiting the
generated page!

You can change the `odoc-config.sexp`, `opam upgrade test_odoc_driver` and rerun
the driver to see how that affects the `-P` and `-L` values passed.
