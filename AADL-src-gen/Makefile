BUNDLES_DIR = $(shell pwd)/bundles
BUNDLES = $(wildcard $(BUNDLES_DIR)/*)
FLAGS = $(foreach d, $(BUNDLES),-cflags '-I $d' \
	-lflags '-I $d $(shell basename $d).cmxa') -package stdcompat -package parmap
BYTE_FLAGS = $(foreach d, $(BUNDLES),-cflags '-I $d' \
	-lflags '-I $d $(shell basename $d).cma') -package stdcompat -package parmap
OCAMLBUILD = ocamlbuild -use-ocamlfind
OCAMLC = ocamlc
EXECUTABLE = ardu_c_to_aadl
INSTALL_DIR_PREFIX = /usr/local
INSTALL_DIR = $(INSTALL_DIR_PREFIX)/lib/spinfer
BIN_FILE = $(INSTALL_DIR_PREFIX)/bin/spinfer
MAN_DIR = /usr/local/share/man/man1
OCAML_INCLUDES = -I _build/contrib/coccinelle/parsing_c \
	-I _build/contrib/coccinelle/commons \
	-I _build/contrib/coccinelle/tools \
	-I _build/contrib/coccinelle/globals \
	-I _build/contrib/coccinelle \
	-I _build/contrib/coccinelle/engine \
	-I _build/contrib/coccinelle/parsing_cocci \
	-I _build/contrib/coccinelle/ctl \
	-I _build/contrib/spinfer \
	-I _build/contrib/spinfer/control-graph
GRAPH_OUTPUT_FOLDER = graphs
PATCHES_OUTPUT_FILE = patterns.txt
GITINSPECTOR = gitinspector

build:
	$(OCAMLBUILD) $(FLAGS) $(EXECUTABLE).native

build_debug:
	$(OCAMLBUILD) $(FLAGS) -cflag -g $(EXECUTABLE).native

build_byte:
	$(OCAMLBUILD) $(BYTE_FLAGS) -cflag -g $(EXECUTABLE).d.byte

all: build

clean:
	rm -rf _build/

veryclean: clean
	rm -rf bundles/

.PHONY: all clean veryclean
