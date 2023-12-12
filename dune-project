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
  (decompress
   (>= 1.5.3))
  (bigstringaf
   (>= 0.9.1))
  (base64
   (>= 3.5.1))
  (lwt
   (>= 5.7.0))
  (fpath
   (>= 0.7.3))
  odoc
  (opam-core
   (>= 2.1.5))
  (tyxml
   (>= 4.6.0))
  (brr
   (>= 0.0.6))
  (alcotest :with-test))
 (depopts ancient))

(package
 (name sherlodoc-www)
 (synopsis "Website for fuzzy search in OCaml documentation")
 (depends
  (ocaml
   (>= 4.14.0))
  dune
  sherlodoc
  (ancient
   (>= 0.9.1))
  (dream
   (>= 1.0.0~alpha5))
  (alcotest :with-test)))
