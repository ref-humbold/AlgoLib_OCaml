DIR = _build/install/default/lib/algolib
BIN = bin
CMXA = algolib.cmxa

.PHONY : all clean refresh

all : algolib

clean :
	rm -fr $(BIN)
	dune clean

refresh : clean all

algolib :
	dune build
	@mkdir -p $(BIN)
	@ln -sfn ../$(DIR)/$(CMXA) $(BIN)/$(CMXA)
