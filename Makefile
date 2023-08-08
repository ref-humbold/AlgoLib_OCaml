BUILD_SRC = _build/default/src
BUILD_DOC_HTML = _build/default/_doc/_html

SRC = src
TEST = test

OUTPUT = buildOut
DIST = $(OUTPUT)/dist
DOCS = $(OUTPUT)/docs

CMXA_LIBRARY = algolib.cmxa
CMA_LIBRARY = algolib.cma

.PHONY : all build clean compile doc format refresh refresh-all test

all : build test

clean :
	rm -fr $(DOUTPUT)
	dune clean

format :
	find $(SRC) -regex .+\.mli? -exec ocamlformat -i {} \; -exec ocp-indent -i {} \;
	find $(TEST) -regex .+\.mli? -exec ocamlformat -i {} \; -exec ocp-indent -i {} \;

compile :
	mkdir -p $(DIST)
	dune build
	cp $(BUILD_SRC)/$(CMXA_LIBRARY) $(DIST)/$(CMXA_LIBRARY)
	cp $(BUILD_SRC)/$(CMA_LIBRARY) $(DIST)/$(CMA_LIBRARY)

test :
	dune runtest

build : format compile

refresh : clean build

refresh-all : clean all

doc :
	dune build @doc
	cp -r $(BUILD_DOC_HTML) $(DOCS)
