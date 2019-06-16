BUILD_SRC = _build/default/src
BUILD_DOC_HTML = _build/default/_doc/_html
ALGOLIB_CMXA = algolib.cmxa
ALGOLIB_CMA = algolib.cma
BIN = bin
DOC = doc
SRC = src
TEST = test

.PHONY : all clean compile doc format refresh test

all : format compile test

clean :
	rm -fr $(BIN) $(DOC)
	dune clean

refresh : clean all

compile :
	dune build
	mkdir -p $(BIN)
	ln -sfn ../$(BUILD_SRC)/$(ALGOLIB_CMXA) $(BIN)/$(ALGOLIB_CMXA)
	ln -sfn ../$(BUILD_SRC)/$(ALGOLIB_CMA) $(BIN)/$(ALGOLIB_CMA)

test :
	dune runtest

format :
	dune build @fmt --auto-promote > /dev/null 2> /dev/null; [ $$? -le 1 ]
	for F in $$(find $(SRC) -regextype egrep -regex '.+\.mli?'); do ocp-indent -i $$F; done
	for F in $$(find $(TEST) -regextype egrep -regex '.+\.mli?'); do ocp-indent -i $$F; done

doc :
	dune build @doc
	ln -sfn $(BUILD_DOC_HTML) $(DOC)
