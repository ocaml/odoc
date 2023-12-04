(lang dune 2.9)

(cram enable)

(using menhir 2.1)

(generate_opam_files true)

(name sherlodoc)

(source
 (github art-w/sherlodoc))

(authors "Arthur Wendling")

(maintainers "art.wendling@gmail.com")

(license MIT)

(package
 (name sherlodoc)
 (synopsis "Fuzzy search in OCaml documentation")
 (depends
  (ocaml
   (>= 4.14.0))
  dune
  decompress
  bigstringaf
  base64
  fpath
  odoc
  opam-core
  (tyxml (>= 4.6.0))
  brr
  (alcotest :with-test)))

(package
 (name sherlodoc-www)
 (synopsis "Website for fuzzy search in OCaml documentation")
 (depends
  (ocaml
   (>= 4.14.0))
  dune
  sherlodoc
  ancient
  dream
  (alcotest :with-test)))
