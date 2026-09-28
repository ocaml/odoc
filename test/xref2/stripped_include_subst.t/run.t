Inline include decls are stripped to empty signatures after compilation.
Substituting through such a module type (`A.S with type t := int`) must
still apply to the contents of the stripped include.

Set up:

  $ cat a.mli
  (** Module A defines a signature whose inline include declares the type. *)
  
  module type S = sig
    include sig
      type t
  
      val x : t
      val y : t -> t
    end
  end



  $ cat b.mli
  (** Module B includes [A.S] with a type substitution. *)
  
  module M : A.S with type t := int



Compile and link:

  $ compile a.mli b.mli

The module M in B should have both vals from the include, with
type t substituted for int:

  $ odoc_print b.odocl -r M.x | jq -c '.type_'
  {"Constr":[{"`Resolved":{"`CoreType":"int"}},[]]}

  $ odoc_print b.odocl -r M.y | jq -c '.type_'
  {"Arrow":["None",[{"Constr":[{"`Resolved":{"`CoreType":"int"}},[]]},[]],[{"Constr":[{"`Resolved":{"`CoreType":"int"}},[]]},[]]]}

Generate HTML and verify both vals appear:

  $ odoc html-generate b.odocl -o html --indent
  $ grep 'id="val-[xy]"' html/test/B/M/index.html
        <div class="spec value anchored" id="val-x">
        <div class="spec value anchored" id="val-y">
