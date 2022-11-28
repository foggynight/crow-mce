PROG=crow
SRCS=crow.scm

.PHONY: all
all:
	csc -o $(PROG) -d3 $(SRCS)

.PHONY: fast
fast:
	csc -o $(PROG) -O5 -d0 $(SRCS)
