##############################
# SHARED VARIABLES AND RULES #
##############################

ODOC = ../_build/bin/main.native

COMP_FLAGS = --pkg=$(PKG) -I $(DIR) -o $(DIR)

%.html: %.odoc
	$(ODOC) html -I $(DIR) -o $(DIR) $(DIR)/$<

################################
# DEFAULT VALUE FOR OUTPUT DIR #
################################

PKG = odoc
DIR = ./odoc

all: compile link coverage

clean: clean_odoc clean_coverage

##########################
# TESTING ON ODOC ITSELF #
##########################

LIB  = ../_build/lib

UNITS = odoc odocFs odocRoot odocUnit odocEnv odocCompile odocHtml

HTML_FILES = $(UNITS:=.html)
ODOC_FILES = $(UNITS:=.odoc)

compile: DIR = ./odoc
compile: PKG = odoc
compile: $(ODOC_FILES)

do-link: DIR = ./odoc
do-link: $(HTML_FILES)
	@cp odoc.css $(DIR)

do-link: DIR = ./odoc
link:
	# $(ODOC) compile -r $(COMP_FLAGS) $(LIB)/odoc.cmt
	$(MAKE) do-link

odoc.odoc:
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odoc.cmt

odocFs.odoc: odoc.odoc
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odocFs.cmti

odocRoot.odoc: odoc.odoc
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odocRoot.cmti

odocUnit.odoc: odoc.odoc odocFs.odoc odocRoot.odoc
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odocUnit.cmti

odocEnv.odoc: odoc.odoc odocFs.odoc odocRoot.odoc odocUnit.odoc
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odocEnv.cmti

odocCompile.odoc: odoc.odoc odocEnv.odoc odocFs.odoc odocRoot.odoc
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odocCompile.cmti

odocHtml.odoc: odoc.odoc odocFs.odoc odocEnv.odoc
	$(ODOC) compile $(COMP_FLAGS) $(LIB)/odocHtml.cmti

clean_odoc: DIR = ./odoc
clean_odoc:
	-@rm -R $(DIR)
	-@mkdir $(DIR)

#################################
# TEST ON DOC-OCK COVERAGE FILE #
#################################

coverage: DIR = ./coverage
coverage: PKG = coverage
coverage: coverage.html
	@cp odoc.css $(DIR)

coverage.odoc: DIR = ./coverage
coverage.odoc: coverage.cmti
	$(ODOC) compile $(COMP_FLAGS) $(DIR)/coverage.cmti

coverage.cmti: DIR = ./coverage
coverage.cmti:
	ocamlc -bin-annot $(DIR)/coverage.mli

clean_coverage: DIR = ./coverage
clean_coverage:
	-@rm $(DIR)/odoc.css
	-@rm $(DIR)/coverage.odoc
	-@rm $(DIR)/coverage.cm*
	-@rm -R $(DIR)/Coverage

################################
################################

.PHONY: all clean clean_odoc clean_coverage compile link coverage
