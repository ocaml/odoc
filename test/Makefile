
.PHONY: all

OCAMLC ?= ocamlc -g
DOC_OCK_XML_BUILD ?= ../_build/lib-doc-ock-xml
DOC_OCK_FLAGS ?= \
   -I  $(DOC_OCK_XML_BUILD) \
   $(shell ocamlfind query -r -predicates byte -format "-I %d" -r doc-ock) \
   $(shell ocamlfind query -r -predicates byte -format "-I %d" -r xmlm) \
   $(shell ocamlfind query -r -predicates byte -format "%d/%a" -r xmlm) \
   $(shell ocamlfind query -r -predicates byte -format "%d/%a" -r menhirLib) \
   $(shell ocamlfind query -r -predicates byte -format "%d/%a" -r doc-ock) \
   $(DOC_OCK_XML_BUILD)/doc-ock-xml.cma

all: coverage-cmi.out coverage-cmti.out coverage-cmt.out

coverage-cmi.out: coverage.cmi testCmi
	./testCmi coverage.cmi > coverage-cmi.out

coverage-cmti.out: coverage.cmti testCmti
	./testCmti coverage.cmti > coverage-cmti.out

coverage-cmt.out: coverage.cmt testCmt
	./testCmt coverage.cmt > coverage-cmt.out

coverage.cmi coverage.cmti: coverage.mli
	$(OCAMLC) -bin-annot -c coverage.mli

coverage.cmt: coverage.ml
	$(OCAMLC) -bin-annot -c coverage.ml

testCmi: testCmi.ml
	$(OCAMLC) $(DOC_OCK_FLAGS) $< -o $@

testCmti: testCmti.ml
	$(OCAMLC) $(DOC_OCK_FLAGS) $< -o $@

testCmt: testCmt.ml
	$(OCAMLC) $(DOC_OCK_FLAGS) $< -o $@

clean:
	rm -f testCmi
	rm -f testCmti
	rm -f testCmt
	rm -f coverage-cmi.out
	rm -f coverage-cmti.out
	rm -f coverage-cmt.out
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.cmti
	rm -f *.cmt
