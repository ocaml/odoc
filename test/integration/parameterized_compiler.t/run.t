This test uses OxCaml parameterised libraries. It builds the libraries with the
OxCaml compiler, not with dune, because dune cannot make some of the instances
that the test needs.

These are the terms that the test uses:

- A library parameter is an interface. It has no implementation.
- A parameterised library uses a library parameter.
- An argument is an implementation of a library parameter.
- An instance is a parameterised library, with an argument for each parameter.
In the source code, you write the instance of `Lib` with the argument `A` for
the parameter `P` as `Lib(P)(A)`. Odoc shows it as `Lib[P:A]`.

These are the units of the test:

- `P` and `Q` are library parameters.
- `P_impl` is an argument for `P`. `Q_impl` is an argument for `Q`.
- `P_of_q` is an argument for `P`. It also has the parameter `Q`.
- `Hid__p_impl` is an argument for `P`. Its name contains `__`, so odoc usually
hides it.
- `Lib` has the parameter `P`.
- `Hlib__y` has the parameter `P`. Its name contains `__`, so odoc usually hides
it.
- `Pass` has the parameter `P`. It refers to `Lib.w` and `Lib.Inner`.
- `User` makes instances of these libraries.

A unit can refer to `Lib` by its name alone, as in `Lib.w`, only if the unit
also has the parameter `P`. In that unit, `Lib` means `Lib` with the same `P` as
the unit. `Pass` is such a unit. Thus, in the instance `Pass(P)(P_impl)`, `Lib`
also has the argument `P_impl`, but the source code does not show this.

  $ cat > p.mli <<EOF
  > (** The [P] parameter. *)
  > 
  > type t
  > val make : int -> t
  > module Sub : sig type s end
  > EOF
  $ cat > q.mli <<EOF
  > (** The [Q] parameter. *)
  > 
  > type u
  > val of_int : int -> u
  > EOF
  $ cat > p_impl.ml <<EOF
  > type t = int
  > let make n = n
  > module Sub = struct type s = unit end
  > EOF
  $ cat > q_impl.ml <<EOF
  > type u = int
  > let of_int n = n
  > EOF
  $ cat > p_of_q.ml <<EOF
  > type t = Q.u
  > let make n = Q.of_int n
  > module Sub = struct type s = Q.u end
  > EOF
  $ cat > hid__p_impl.ml <<EOF
  > type t = char
  > let make _ = 'x'
  > module Sub = struct type s = char end
  > EOF
  $ cat > lib.ml <<EOF
  > type w = { value : P.t }
  > let wrap value = { value }
  > module Inner = struct type i = P.Sub.s end
  > EOF
  $ cat > hlib__y.ml <<EOF
  > type w = { value : P.t }
  > EOF
  $ cat > pass.ml <<EOF
  > type implicit = Lib.w
  > module Inner = Lib.Inner
  > EOF
  $ cat > user.ml <<EOF
  > module Nested = Lib(P)(P_of_q(Q)(Q_impl)) [@jane.non_erasable.instances]
  > module Passed = Pass(P)(P_impl) [@jane.non_erasable.instances]
  > module Hidden = Lib(P)(Hid__p_impl) [@jane.non_erasable.instances]
  > module Hidden_lib = Hlib__y(P)(P_impl) [@jane.non_erasable.instances]
  > type nested = Nested.w
  > type inner = Nested.Inner.i
  > type passed_implicit = Passed.implicit
  > type passed_inner = Passed.Inner.i
  > type hidden = Hidden.w
  > type hidden_lib = Hidden_lib.w
  > EOF

Compile each unit. The flags tell the compiler what each unit is:
`-as-parameter` makes a library parameter, `-as-argument-for` makes an
argument, and `-parameter` gives a parameter to a library:

  $ c() { ocamlc -bin-annot -w -misplaced-attribute -w -bad-module-name -c "$@"; }
  $ c -as-parameter p.mli
  $ c -as-parameter q.mli
  $ c -as-argument-for P p_impl.ml
  $ c -as-argument-for Q q_impl.ml
  $ c -parameter Q -as-argument-for P p_of_q.ml
  $ c -as-argument-for P hid__p_impl.ml
  $ c -parameter P lib.ml
  $ c -parameter P hlib__y.ml
  $ c -parameter P pass.ml
  $ c user.ml

Make the documentation. For each unit, odoc compiles, links and makes HTML:

  $ units="p q p_impl q_impl p_of_q hid__p_impl lib hlib__y pass user"
  $ for u in $units; do
  >   f=$u.cmti; [ -f $f ] || f=$u.cmt
  >   odoc compile --enable-missing-root-warning -I . $f -o $u.odoc
  > done
  $ for u in $units; do
  >   odoc link --enable-missing-root-warning -I . $u.odoc -o $u.odocl
  > done
  $ for u in $units; do odoc html-generate --indent -o html $u.odocl; done

The function `decls` shows the declarations on an HTML page. It shows each link
as `[text -> target]`:

  $ decls() {
  >   awk '/class="spec /{p=1} p && /<code>/{c=1; s=""} c{s=s " " $0} c && /<\/code>/{print s; c=0; p=0}' "$1" |
  >   sed -E -e 's#<a href="([^"]*)"[^>]*>([^<]*)</a>#[\2 -> \1]#g' -e 's/<[^>]*>//g' \
  >     -e 's/&gt;/>/g; s/&lt;/</g; s/&amp;/\&/g' -e 's/  +/ /g; s/^ //; s/ $//'
  > }

The page of `User`:

- `Nested` is an instance of `Lib`. Its argument is also an instance: `P_of_q`
with the argument `Q_impl` for `Q`.
- `Passed` is an instance of `Pass`.
- `Hidden` is an instance of `Lib` with a hidden argument.
- `Hidden_lib` is an instance of a hidden library.

Dune always makes an instance from the main module of each library. Thus, dune
never puts a hidden unit in an instance. But you can name a hidden unit in the
source code, as `User` does.

Odoc expands `Hidden_lib`, because `Hlib__y` is hidden. Odoc expands an alias
to a hidden module in the same way. Odoc does not expand `Hidden`:

  $ decls html/User/index.html
  module Nested = [Lib[P:P_of_q[Q:Q_impl]] -> ../Lib/index.html]
  module Passed = [Pass[P:P_impl] -> ../Pass/index.html]
  module Hidden = [Lib[P:Hid__p_impl] -> ../Lib/index.html]
  module [Hidden_lib -> Hidden_lib/index.html] : sig ... end
  type nested = [Nested.w -> ../Lib/index.html#type-w]
  type inner = [Nested.Inner.i -> ../Lib/Inner/index.html#type-i]
  type passed_implicit = [Passed.implicit -> ../Pass/index.html#type-implicit]
  type passed_inner = [Passed.Inner.i -> ../Lib/Inner/index.html#type-i]
  type hidden = [Hidden.w -> Hidden/index.html#type-w]
  type hidden_lib = [Hidden_lib.w -> Hidden_lib/index.html#type-w]

FIXME: The link for `Hidden.w` goes to an expansion of `User.Hidden`. Odoc
does not make this page:

  $ ls html/User
  Hidden_lib
  index.html

The page of `Pass`:

`Pass` refers to `Lib` by its name alone. Thus, odoc shows `Lib`, not an
instance of `Lib`:

  $ decls html/Pass/index.html
  parameter [P -> ../P/index.html]
  type implicit = [Lib.w -> ../Lib/index.html#type-w]
  module Inner = [Lib.Inner -> ../Lib/Inner/index.html]
