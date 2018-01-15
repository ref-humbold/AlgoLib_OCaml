CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLFLAGS = -w A
CMPL = $(CAMLOPT) $(CAMLFLAGS)
SRC = src
OBJ = obj
ALGOLIB = $(SRC)/algolib
MATHS = $(ALGOLIB)/maths
STRUCTURES = $(ALGOLIB)/structures

all : prepare srcDir

prepare :
	mkdir -p $(OBJ)

clean :
	rm -rf $(OBJ)

refresh : clean all

srcDir : algolibDir mathsDir structuresDir

algolibDir : convex_hull.cmx maximum_subarray.cmx sorting.cmx

mathsDir : maths.cmx

structuresDir : binomial_heap.cmx delay.cmx deque.cmx disjoint_sets.cmx hood_melville_queue.cmx \
  indexed_list.cmx leftist_heap.cmx pairing_heap.cmx red_black_tree.cmx skew_indexed_list.cmx

# algolib

convex_hull.cmx : $(ALGOLIB)/convex_hull.ml
	$(CMPL) -c $(ALGOLIB)/convex_hull.ml -o convex_hull.cmx
	@mv $(ALGOLIB)/convex_hull.cmi $(ALGOLIB)/convex_hull.cmx $(ALGOLIB)/convex_hull.o $(OBJ)

maximum_subarray.cmx : $(ALGOLIB)/maximum_subarray.ml
	$(CMPL) -c $(ALGOLIB)/maximum_subarray.ml -o maximum_subarray.cmx
	@mv $(ALGOLIB)/maximum_subarray.cmi $(ALGOLIB)/maximum_subarray.cmx \
	  $(ALGOLIB)/maximum_subarray.o $(OBJ)

sorting.cmx : $(ALGOLIB)/sorting.ml
	$(CMPL) -c $(ALGOLIB)/sorting.ml -o sorting.cmx
	@mv $(ALGOLIB)/sorting.cmi $(ALGOLIB)/sorting.cmx $(ALGOLIB)/sorting.o $(OBJ)

# algolib/maths

maths.cmx : $(MATHS)/maths.ml
	$(CMPL) -c $(MATHS)/maths.ml -o maths.cmx
	@mv $(MATHS)/maths.cmi $(MATHS)/maths.cmx $(MATHS)/maths.o $(OBJ)

# algolib/structures

binomial_heap.cmx : $(STRUCTURES)/binomial_heap.ml
	$(CMPL) -I $(STRUCTURES) -c $(STRUCTURES)/binomial_heap.ml -o binomial_heap.cmx
	@mv $(STRUCTURES)/binomial_heap.cmi $(STRUCTURES)/binomial_heap.cmx \
	  $(STRUCTURES)/binomial_heap.o $(OBJ)

delay.cmx : $(STRUCTURES)/delay.mli $(STRUCTURES)/delay.ml
	$(CMPL) -c $(STRUCTURES)/delay.mli -o delay.cmi
	$(CMPL) -I $(STRUCTURES) -c $(STRUCTURES)/delay.ml -o delay.cmx
	@mv $(STRUCTURES)/delay.cmi $(STRUCTURES)/delay.cmx $(STRUCTURES)/delay.o $(OBJ)

deque.cmx : $(STRUCTURES)/deque.mli $(STRUCTURES)/deque.ml
	$(CMPL) -c $(STRUCTURES)/deque.mli -o deque.cmi
	$(CMPL) -I $(STRUCTURES) -c $(STRUCTURES)/deque.ml -o deque.cmx
	@mv $(STRUCTURES)/deque.cmi $(STRUCTURES)/deque.cmx $(STRUCTURES)/deque.o $(OBJ)

disjoint_sets.cmx : $(STRUCTURES)/disjoint_sets.ml
	$(CMPL) -I $(STRUCTURES) -c $(STRUCTURES)/disjoint_sets.ml -o disjoint_sets.cmx
	@mv $(STRUCTURES)/disjoint_sets.cmi $(STRUCTURES)/disjoint_sets.cmx \
	  $(STRUCTURES)/disjoint_sets.o $(OBJ)

hood_melville_queue.cmx : $(STRUCTURES)/hood_melville_queue.mli $(STRUCTURES)/hood_melville_queue.ml
	$(CMPL) -c $(STRUCTURES)/hood_melville_queue.mli -o hood_melville_queue.cmi
	$(CMPL) -I $(STRUCTURES) -c $(STRUCTURES)/hood_melville_queue.ml -o hood_melville_queue.cmx
	@mv $(STRUCTURES)/hood_melville_queue.cmi $(STRUCTURES)/hood_melville_queue.cmx \
	  $(STRUCTURES)/hood_melville_queue.o $(OBJ)

indexed_list.cmx : $(STRUCTURES)/indexed_list.mli $(STRUCTURES)/indexed_list.ml
	$(CMPL) -c $(STRUCTURES)/indexed_list.mli -o indexed_list.cmi
	$(CMPL) -I $(STRUCTURES) -c $(STRUCTURES)/indexed_list.ml -o indexed_list.cmx
	@mv $(STRUCTURES)/indexed_list.cmi $(STRUCTURES)/indexed_list.cmx \
	  $(STRUCTURES)/indexed_list.o $(OBJ)

leftist_heap.cmx : $(STRUCTURES)/leftist_heap.ml
	$(CMPL) -I $(STRUCTURES) -c $(STRUCTURES)/leftist_heap.ml -o leftist_heap.cmx
	@mv $(STRUCTURES)/leftist_heap.cmi $(STRUCTURES)/leftist_heap.cmx $(STRUCTURES)/leftist_heap.o \
	  $(OBJ)

pairing_heap.cmx : $(STRUCTURES)/pairing_heap.ml
	$(CMPL) -I $(STRUCTURES) -c $(STRUCTURES)/pairing_heap.ml -o pairing_heap.cmx
	@mv $(STRUCTURES)/pairing_heap.cmi $(STRUCTURES)/pairing_heap.cmx $(STRUCTURES)/pairing_heap.o \
	  $(OBJ)

red_black_tree.cmx : $(STRUCTURES)/red_black_tree.ml
	$(CMPL) -I $(STRUCTURES) -c $(STRUCTURES)/red_black_tree.ml -o red_black_tree.cmx
	@mv $(STRUCTURES)/red_black_tree.cmi $(STRUCTURES)/red_black_tree.cmx \
	  $(STRUCTURES)/red_black_tree.o $(OBJ)

skew_indexed_list.cmx : $(STRUCTURES)/skew_indexed_list.mli $(STRUCTURES)/skew_indexed_list.ml
	$(CMPL) -c $(STRUCTURES)/skew_indexed_list.mli -o skew_indexed_list.cmi
	$(CMPL) -I $(STRUCTURES) -c $(STRUCTURES)/skew_indexed_list.ml -o skew_indexed_list.cmx
	@mv $(STRUCTURES)/skew_indexed_list.cmi $(STRUCTURES)/skew_indexed_list.cmx \
	  $(STRUCTURES)/skew_indexed_list.o $(OBJ)
