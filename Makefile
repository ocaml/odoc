
.PHONY: all clean install

BUILDIR ?= _build
ROOTDIR = $(shell pwd)

OCAMLC ?= ocamlc -bin-annot
OCAMLOPT ?= ocamlopt -bin-annot
MENHIR ?= menhir
LN ?= ln -sf
MKDIR ?= mkdir -p

BYTE_PKGS ?= $(shell ocamlfind query -r -predicates byte -format "-I %d" -r xmlm \
                 doc-ock-lib \
                 menhirLib) \
             -I $(BUILDIR)/lib-doc-ock-xml

NATIVE_PKGS ?= $(shell ocamlfind query -r -predicates native -format "-I %d" -r xmlm \
                   doc-ock-lib \
                   menhirLib) \
               -I $(BUILDIR)/lib-doc-ock-xml

all: $(BUILDIR)/lib-doc-ock-xml/doc-ock-xml.cma $(BUILDIR)/lib-doc-ock-xml/doc-ock-xml.cmxa $(BUILDIR)/lib-doc-ock-xml/doc-ock-xml.a $(BUILDIR)/lib-doc-ock-xml/doc-ock-xml.cmxs

$(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.cmi: $(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.mli
	$(OCAMLC) $(BYTE_PKGS) -c $<

$(BUILDIR)/lib-doc-ock-xml/docOckXmlParse.cmi: $(BUILDIR)/lib-doc-ock-xml/docOckXmlParse.mli $(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.cmi
	$(OCAMLC) $(BYTE_PKGS) -c $<

$(BUILDIR)/lib-doc-ock-xml/docOckXmlFold.cmi: $(BUILDIR)/lib-doc-ock-xml/docOckXmlFold.mli
	$(OCAMLC) $(BYTE_PKGS) -c $<

$(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.cmo: $(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.ml $(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.cmi
	$(OCAMLC) $(BYTE_PKGS) -c $<

$(BUILDIR)/lib-doc-ock-xml/docOckXmlParse.cmo: $(BUILDIR)/lib-doc-ock-xml/docOckXmlParse.ml $(BUILDIR)/lib-doc-ock-xml/docOckXmlParse.cmi $(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.cmi
	$(OCAMLC) $(BYTE_PKGS) -c $<

$(BUILDIR)/lib-doc-ock-xml/docOckXmlFold.cmo: $(BUILDIR)/lib-doc-ock-xml/docOckXmlFold.ml $(BUILDIR)/lib-doc-ock-xml/docOckXmlFold.cmi
	$(OCAMLC) $(BYTE_PKGS) -c $<

$(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.cmx: $(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.ml
	$(OCAMLOPT) $(NATIVE_PKGS) -c $<

$(BUILDIR)/lib-doc-ock-xml/docOckXmlParse.cmx: $(BUILDIR)/lib-doc-ock-xml/docOckXmlParse.ml $(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.cmi
	$(OCAMLOPT) $(NATIVE_PKGS) -c $<

$(BUILDIR)/lib-doc-ock-xml/docOckXmlFold.cmx: $(BUILDIR)/lib-doc-ock-xml/docOckXmlFold.ml
	$(OCAMLOPT) $(NATIVE_PKGS) -c $<

$(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.mli $(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.ml: $(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.mly
	$(MENHIR) $<

$(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.mly: $(ROOTDIR)/src/docOckXmlParser.mly | $(BUILDIR)/lib-doc-ock-xml/
	$(LN) $< $@

$(BUILDIR)/lib-doc-ock-xml/docOckXmlParse.ml: $(ROOTDIR)/src/docOckXmlParse.ml | $(BUILDIR)/lib-doc-ock-xml/
	$(LN) $< $@

$(BUILDIR)/lib-doc-ock-xml/docOckXmlFold.ml: $(ROOTDIR)/src/docOckXmlFold.ml | $(BUILDIR)/lib-doc-ock-xml/
	$(LN) $< $@

$(BUILDIR)/lib-doc-ock-xml/docOckXmlParse.mli: $(ROOTDIR)/src/docOckXmlParse.mli | $(BUILDIR)/lib-doc-ock-xml/
	$(LN) $< $@

$(BUILDIR)/lib-doc-ock-xml/docOckXmlFold.mli: $(ROOTDIR)/src/docOckXmlFold.mli | $(BUILDIR)/lib-doc-ock-xml/
	$(LN) $< $@

$(BUILDIR)/lib-doc-ock-xml/doc-ock-xml.cma:  \
    $(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.cmo \
    $(BUILDIR)/lib-doc-ock-xml/docOckXmlParse.cmo \
    $(BUILDIR)/lib-doc-ock-xml/docOckXmlFold.cmo \
    | $(BUILDIR)/lib-doc-ock-xml/
	$(OCAMLC) -a $^ -o $@

$(BUILDIR)/lib-doc-ock-xml/doc-ock-xml.cmxa $(BUILDIR)/lib-doc-ock-xml/doc-ock-xml.a:  \
    $(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.cmx \
    $(BUILDIR)/lib-doc-ock-xml/docOckXmlParse.cmx \
    $(BUILDIR)/lib-doc-ock-xml/docOckXmlFold.cmx \
    | $(BUILDIR)/lib-doc-ock-xml/
	$(OCAMLOPT) -a $^ -o $@

$(BUILDIR)/lib-doc-ock-xml/doc-ock-xml.cmxs:  \
    $(BUILDIR)/lib-doc-ock-xml/docOckXmlParser.cmx \
    $(BUILDIR)/lib-doc-ock-xml/docOckXmlParse.cmx \
    $(BUILDIR)/lib-doc-ock-xml/docOckXmlFold.cmx \
    | $(BUILDIR)/lib-doc-ock-xml/
	$(OCAMLOPT) -shared -linkall $^ -o $@

$(BUILDIR)/lib-doc-ock-xml/:
	$(MKDIR) $(BUILDIR)/lib-doc-ock-xml

install:: all
	opam-installer --prefix $(shell opam config var prefix) doc-ock-xml.install

clean::
	rm -f *~ **/*~
	rm -rf $(BUILDIR)
