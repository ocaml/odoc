MODULES=docOckHtml docOckHtmlMarkup docOckHtmlHtml_tree docOckHtmlTo_html_tree \
	docOckHtmlList_targets

PKGS = tyxml,doc-ock,xmlm
FLAGS = -open StdLabels -w -40 -bin-annot -g

all: doc-ock-html.cma doc-ock-html.cmxa

CMIS=$(MODULES:=.cmi)
CMOS=$(MODULES:=.cmo)
CMXS=$(MODULES:=.cmx)
OS=$(MODULES:=.o)

docOckHtml.cmi: docOckHtml.ml
	ocamlfind ocamlc $(FLAGS) -w -49 -c -no-alias-deps $<

docOckHtml.cmx: docOckHtml.ml
	ocamlfind ocamlopt $(FLAGS) -w -49 -c -no-alias-deps $<

%.cmi: %.mli %.ml
	ocamlfind ocamlc $(FLAGS) -open DocOckHtml -package $(PKGS) -c $^

%.cmo: %.cmi

%.cmx: %.mli %.ml
	ocamlfind ocamlopt $(FLAGS) -open DocOckHtml -package $(PKGS) -c $^

doc-ock-html.cma: $(CMIS) $(CMOS)
	ocamlfind ocamlc -package $(PKGS) $(FLAGS) $(CMOS) -a -o $@

doc-ock-html.cmxa: $(CMIS) $(CMXS)
	ocamlopt $(CMXS) -a -o $@


clean:
	-@rm *.cm*
	-@rm *.o
	-@rm *.a

install: all
	ocamlfind install doc-ock-html META \
	    $(CMIS) $(CMOS) $(CMXS) \
	    doc-ock-html.cma doc-ock-html.cmxa doc-ock-html.a

uninstall:
	ocamlfind remove doc-ock-html

reinstall:
	-@make clean
	-@make uninstall
	make install

.PHONY: all byte native clean install
