(lang dune 3.18)

(name odoc)

(documentation "https://ocaml.org/p/odoc")

(source
 (github ocaml/odoc))

(license ISC)

(authors
 "Anton Bachin <antonbachin@yahoo.com>"
 "Daniel B\195\188nzli <daniel.buenzli@erratique.ch>"
 "David Sheets <sheets@alum.mit.edu>"
 "Jon Ludlam <jon@recoil.org>"
 "Jules Aguillon <juloo.dsi@gmail.com>"
 "Leo White <leo@lpw25.net>"
 "Lubega Simon <lubegasimon73@gmail.com>"
 "Paul-Elliot Angl\195\168s d'Auriac <paul-elliot@tarides.com>"
 "Thomas Refis <trefis@janestreet.com>"
 "Arthur Wendling <art.wendling@gmail.com>"
 "Emile Trotignon <emile.trotignon@gmail.com>")

(maintainers
 "Daniel B\195\188nzli <daniel.buenzli@erratique.ch>"
 "Jon Ludlam <jon@recoil.org>"
 "Jules Aguillon <juloo.dsi@gmail.com>"
 "Paul-Elliot Angl\195\168s d'Auriac <paul-elliot@tarides.com>")

(cram enable)

(using mdx 0.3)

; Sherlodoc
(using menhir 2.1)
(using directory-targets 0.1)
