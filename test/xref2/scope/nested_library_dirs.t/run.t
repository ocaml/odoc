A library's [.odoc] files may sit in a directory below another library's, as
they do when a driver lays them out like the switch: findlib puts [lwt] in
[lib/lwt] and [lwt.unix] in [lib/lwt/unix]. Here [p] is in [h/p], and
[p.sub], which requires [p], in [h/p/sub]. The two directories overlap, so
[odoc link] is given [--custom-layout].

  $ cat p/p.mli
  type t
  $ cat sub/other.mli
  type t
  $ cat sub/sub.mli
  (** See {!//Other}. *)
  type t
  $ (cd p && ocamlc -bin-annot -c p.mli)
  $ (cd sub && ocamlc -bin-annot -I ../p -c other.mli sub.mli)
  $ odoc compile -o h/p/p.odoc --parent-id pkg/p -L p:h/p p/p.cmti
  $ odoc compile -o h/p/sub/other.odoc --parent-id pkg/sub -L p:h/p -L p.sub:h/p/sub sub/other.cmti
  $ odoc compile -o h/p/sub/sub.odoc --parent-id pkg/sub -L p:h/p -L p.sub:h/p/sub sub/sub.cmti

A unit belongs to the library whose directory holds it most closely: [Sub]
is in [p.sub], although [h/p] holds it too. So [{!//Other}], a reference
below the current library's directory, should resolve to [p.sub]'s
[Other], whichever order the [-L] options come in. A warning that
[//Other] is not found would be wrong: it would mean [Other] was looked
for below [h/p].

  $ odoc link --custom-layout -L p:h/p -L p.sub:h/p/sub h/p/sub/sub.odoc
  File "sub.mli", line 1, characters 8-18:
  Warning: Failed to resolve reference //Other Path '//Other' not found
  $ odoc link --custom-layout -L p.sub:h/p/sub -L p:h/p h/p/sub/sub.odoc
