# Makefile - CROW Metacircular Evaluator

PRG := crow
SRC := crow.scm
INSTALL := /usr/local/bin

.PHONY: all
all:
	csc -o $(PRG) -O5 -d0 $(SRC)

.PHONY: debug
debug:
	csc -o $(PRG) -d3 $(SRC)

.PHONY: install
install:
	cp $(PRG) $(INSTALL)

.PHONY: uninstall
uninstall:
	rm $(INSTALL)/$(PRG)

.PHONY: clean
clean:
	rm $(PRG)
