(lang dune 2.9)

(cram enable)

(using menhir 2.1)

(generate_opam_files true)

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
  ancient
  dream
  fpath
  (odoc
   (= 2.1.0))
  opam-core
  tyxml
  (alcotest :with-test)))
