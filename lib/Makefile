MODULES=odoc odocFs odocRoot odocUnit odocEnv odocCompile odocLink odocDepends \
	odocTargets

FLAGS=-bin-annot -g -w -40 -open StdLabels

PKGS= xmlm,doc-ock,doc-ock-xml,doc-ock-html,unix,compiler-libs.common

CMIS=$(MODULES:=.cmi)
CMOS=$(MODULES:=.cmo)
CMXS=$(MODULES:=.cmx)
OS=$(MODULES:=.o)

all: odoc.cma odoc.cmxa

odoc.cma: $(CMIS) $(CMOS)
	ocamlfind ocamlc $(FLAGS) -package $(PKGS) $(CMOS) -a -o $@

odoc.cmxa: $(CMIS) $(CMXS)
	ocamlopt $(CMXS) -a -o $@

clean:
	-@rm *.cm*
	-@rm *.o
	-@rm *.a

odoc.cmi: odoc.ml
	ocamlfind ocamlc $(FLAGS) -w -49 -c -no-alias-deps $<

odoc.cmx: odoc.ml
	ocamlfind ocamlopt $(FLAGS) -w -49 -c -no-alias-deps $<

%.cmi : %.mli %.ml
	ocamlfind ocamlc $(FLAGS) -package $(PKGS) -c $^

%.cmo: %.cmi
	-true

%.cmx : %.mli %.ml
	ocamlfind ocamlopt $(FLAGS) -package $(PKGS) -c $^

.PHONY: all clean
