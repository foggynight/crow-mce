# Makefile - CROW Metacircular Evaluator

NAME := crow
ARGS := -I src
SRCS := src/crow.scm
INSTALL := /usr/local/bin

.PHONY: all
all:
	csc -o $(NAME) -O5 -d0 $(ARGS) $(SRCS)

.PHONY: debug
debug:
	csc -o $(NAME) -d3 $(ARGS) $(SRCS)

.PHONY: install
install:
	cp $(NAME) $(INSTALL)

.PHONY: uninstall
uninstall:
	rm $(INSTALL)/$(NAME)

.PHONY: clean
clean:
	rm $(NAME)
