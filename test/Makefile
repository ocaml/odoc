
.PHONY: all coverage-cmi coverage-cmti

OCAMLC ?= ocamlc -g
DOC_OCK_XML_BUILD ?= ../_build/lib-doc-ock-xml
DOC_OCK_FLAGS ?= \
   -I  $(DOC_OCK_XML_BUILD) \
   $(shell ocamlfind query -r -predicates byte -format "-I %d" -r doc-ock-lib) \
   $(shell ocamlfind query -r -predicates byte -format "-I %d" -r xmlm) \
   $(shell ocamlfind query -r -predicates byte -format "%d/%a" -r xmlm) \
   $(shell ocamlfind query -r -predicates byte -format "%d/%a" -r menhirLib) \
   $(shell ocamlfind query -r -predicates byte -format "%d/%a" -r doc-ock-lib) \
   $(DOC_OCK_XML_BUILD)/doc-ock-xml.cma

all: coverage-cmi coverage-cmti

coverage-cmi: coverage.cmi testCmi
	./testCmi coverage.cmi

coverage-cmti: coverage.cmti testCmti
	./testCmti coverage.cmti

coverage.cmi coverage.cmti: coverage.mli
	$(OCAMLC) -bin-annot -doc -c coverage.mli

testCmi: testCmi.ml
	$(OCAMLC) $(DOC_OCK_FLAGS) $< -o $@

testCmti: testCmti.ml
	$(OCAMLC) $(DOC_OCK_FLAGS) $< -o $@

clean:
	rm -f testCmi
	rm -f testCmti
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.cmti
	rm -f *.cmt
