Two libraries [a] and [b] each have a hidden unit [Hid__den], with different
contents. The units are compiled without [--parent-id], so both units have the
identifier [Hid__den]. The unit [User] of [a] has an alias to its own
[Hid__den].

  $ for l in a b; do
  >   for f in $l/*.mli; do ocamlc -bin-annot -I $l -c $f; done
  > done
  $ for l in a b; do
  >   mkdir -p o/$l
  >   for f in $l/*.cmti; do
  >     odoc compile -I o/$l -o o/$l/$(basename $f .cmti).odoc $f
  >   done
  > done
  $ odoc link -I o/a -I o/b o/a/user.odoc

The identifier does not say which [Hid__den] the alias means. The digest of
the import does, so the expansion of [C] should be the one from [a]. In the
output, the expansion follows [=>]. It should contain [a_only], not [b]'s
[b_only] and [b_also].

  $ odoc_print --short --show-expansions -r C o/a/user.odocl
  module User.C =
    Hid__den
    => sig type t val a_only : t end
