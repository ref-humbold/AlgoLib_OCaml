BUILD_SRC = _build/default/src
BUILD_DOC = _build/default/_doc
BIN = bin
DOC = doc
SRC = src
TEST = test
CMXA = algolib.cmxa
CMA = algolib.cma

.PHONY : all clean refresh test format doc

all : algolib test

clean :
	rm -fr $(BIN) $(DOC)
	dune clean

refresh : clean all

algolib : format
	dune build
	mkdir -p $(BIN)
	ln -sfn ../$(BUILD_SRC)/$(CMXA) $(BIN)/$(CMXA)
	ln -sfn ../$(BUILD_SRC)/$(CMA) $(BIN)/$(CMA)

test :
	dune runtest

format :
	dune build @fmt --auto-promote > /dev/null 2> /dev/null; test $$? -le 1
	for F in $$(find $(SRC) -regextype egrep -regex '.+\.mli?'); do ocp-indent -i $$F; done
	for F in $$(find $(TEST) -regextype egrep -regex '.+\.mli?'); do ocp-indent -i $$F; done

doc :
	dune build @doc
	ln -sfn ./$(BUILD_DOC)/_html $(DOC)
