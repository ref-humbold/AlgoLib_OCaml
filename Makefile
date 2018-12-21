DIR = _build/install/default/lib/algolib
LIB = lib
CMXA = algolib.cmxa

.PHONY : all clean refresh

all : algolib

clean :
	rm -fr $(LIB)
	dune clean

refresh : clean all

algolib :
	dune build
	@mkdir -p $(LIB)
	@ln -sfn ../$(DIR)/$(CMXA) $(LIB)/$(CMXA)
