PROG=crow
SRCS=src/crow.scm
FLAGS=-I src

.PHONY: all
all:
	csc -o $(PROG) -d3 $(SRCS) $(FLAGS)

.PHONY: fast
fast:
	csc -o $(PROG) -O5 -d0 $(SRCS) $(FLAGS)
