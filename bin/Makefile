MODULES=main

FLAGS=-bin-annot -g -open StdLabels

PKGS=xmlm,doc-ock,doc-ock-xml,doc-ock-html,cmdliner,unix
INCLUDES=-I ../lib

CMIS=$(MODULES:=.cmi)
CMOS=$(MODULES:=.cmo)
CMXS=$(MODULES:=.cmx)
OS=$(MODULES:=.o)

#ocamlfind ocamlc $(FLAGS) -package $(PKGS) $(CMOS) -a -o $@

all: odoc.byte odoc.native

odoc.byte: $(CMIS)
	ocamlfind ocamlc $(FLAGS) -package $(PKGS) $(INCLUDES) -linkpkg ../lib/odoc.cma $(CMOS) -o $@

odoc.native: $(CMXS) $(OS)
	ocamlfind ocamlopt $(FLAGS) -package $(PKGS) $(INCLUDES) -linkpkg ../lib/odoc.cmxa $(CMXS) -o $@

clean:
	-@rm *.o
	-@rm *.cm*
	-@rm *.byte
	-@rm *.native

%.cmi : %.ml
	ocamlfind ocamlc $(FLAGS) -package $(PKGS) $(INCLUDES) -c $^

%.cmx : %.ml
	ocamlfind ocamlopt $(FLAGS) -package $(PKGS) $(INCLUDES) -c $^

.PHONY: odoc.byte clean
