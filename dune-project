(lang dune 3.5)

(cram enable)

(using menhir 2.1)

(generate_opam_files true)

(name sherlodoc)

(source (github art-w/sherlodoc))

(authors "Arthur Wendling" "Emile Trotignon")

(maintainers "art.wendling@gmail.com")

(license MIT)

(package
 (name sherlodoc)
 (synopsis "Search engine for OCaml documentation")
 (depends
  (ocaml (>= 4.0.8))
  (odoc (>= 2.4.0))
  (base64 (>= 3.5.1))
  (bigstringaf (>= 0.9.1))
  (js_of_ocaml (>= 5.6.0))
  (brr (>= 0.0.6))
  (cmdliner (>= 1.2.0))
  (decompress (>= 1.5.3))
  (fpath (>= 0.7.3))
  (lwt (>= 5.7.0))
  (menhir (>= 20230608))
  (ppx_blob (>= 0.7.2))
  (tyxml (>= 4.6.0))
  (odig :with-test)
  (base (and :with-test (= v0.16.3)))
  (alcotest :with-test))
 (depopts
  (dream (>= 1.0.0~alpha5))
  (ancient (>= 0.9.1))))
