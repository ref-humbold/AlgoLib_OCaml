BUILD_SRC = _build/default/src
BUILD_CMA = algolib.cma
BUILD_CMXA = algolib.cmxa
BUILD_DOC_HTML = _build/default/_doc/_html

SRC = src
TEST = test

OUTPUT = buildOut
DIST = $(OUTPUT)/dist
DOCS = $(OUTPUT)/docs

CMA_LIBRARY = algolib.cma
CMXA_LIBRARY = algolib.cmxa

.PHONY : all build clean compile doc format refresh refresh-all test

all : build test

clean :
	rm -fr $(OUTPUT)
	dune clean

format :
	find $(SRC) -regex .+\.mli? -exec ocamlformat -i {} \; -exec ocp-indent -i {} \;
	find $(TEST) -regex .+\.mli? -exec ocamlformat -i {} \; -exec ocp-indent -i {} \;

compile :
	mkdir -p $(DIST)
	dune build
	@echo
	cp $(BUILD_SRC)/$(BUILD_CMA) $(DIST)/$(CMA_LIBRARY)
	cp $(BUILD_SRC)/$(BUILD_CMXA) $(DIST)/$(CMXA_LIBRARY)
	chmod 664 $(DIST)/*

build : format compile

test :
	dune runtest

refresh : clean build

refresh-all : clean all

doc :
	mkdir -p $(DOCS)
	dune build @doc
	cp -r $(BUILD_DOC_HTML)/. $(DOCS)
