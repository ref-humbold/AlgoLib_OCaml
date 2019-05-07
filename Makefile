BUILD_SRC = _build/default/src
BUILD_DOC = _build/default/_doc
BIN = bin
DOC = doc
CMXA = algolib.cmxa
CMA = algolib.cma

.PHONY : all clean refresh doc

all : algolib

clean :
	rm -fr $(BIN) $(DOC)
	dune clean

refresh : clean all

algolib :
	dune build
	dune build @fmt --auto-promote > /dev/null 2> /dev/null; test $$? -le 1
	mkdir -p $(BIN)
	ln -sfn ../$(BUILD_SRC)/$(CMXA) $(BIN)/$(CMXA)
	ln -sfn ../$(BUILD_SRC)/$(CMA) $(BIN)/$(CMA)

doc :
	dune build @doc
	ln -sfn ./$(BUILD_DOC)/_html $(DOC)
