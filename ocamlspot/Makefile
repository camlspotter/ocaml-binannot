#########################################################################
#                                                                       #
#                             OCamlSpotter                              #
#                                                                       #
#                              Jun FURUSE                               #
#                                                                       #
#     Copyright 2008-2011 Jun Furuse. All rights reserved.              #
#     This file is distributed under the terms of the GNU Library       #
#     General Public License, with the special exception on linking     #
#     described in file LICENSE.                                        #
#                                                                       #
#########################################################################

include ../config/Makefile

# Various commands and dir
##########################
CAMLRUN=../boot/ocamlrun
OCAMLC   = ../ocamlcomp.sh -annot -w Ae -warn-error Ae
OCAMLOPT = ../ocamlcompopt.sh
OCAMLDEP = $(CAMLRUN) ../tools/ocamldep
OCAMLLEX = $(CAMLRUN) ../boot/ocamllex
OCAMLYACC= ../boot/ocamlyacc
OCAMLLIB = $(LIBDIR)
OCAMLBIN = $(BINDIR)

# Compilation
#############
OCAMLSRCDIR=..
INCLUDES_DEP=-I $(OCAMLSRCDIR)/parsing \
	-I $(OCAMLSRCDIR)/utils \
	-I $(OCAMLSRCDIR)/typing \
	-I $(OCAMLSRCDIR)/driver \
	-I $(OCAMLSRCDIR)/bytecomp \
	-I $(OCAMLSRCDIR)/tools \
	-I $(OCAMLSRCDIR)/toplevel/ 

OTHERS=../otherlibs

# Requires unix!
COMPFLAGS= $(INCLUDES_DEP) -I $(OTHERS)/unix

MODULES= utils dotfile xset treeset command ocaml indexed abstraction spot \
	spoteval spotconfig_intf spotconfig spotfile xmain ocamlspot

UTILS=utils/misc utils/tbl utils/config \
  utils/clflags utils/terminfo utils/ccomp utils/warnings \
  utils/consistbl

PARSING=parsing/linenum parsing/location parsing/longident \
  parsing/syntaxerr parsing/parser \
  parsing/lexer parsing/parse parsing/printast

TYPING=typing/unused_var typing/ident typing/path \
  typing/primitive typing/types \
  typing/btype typing/oprint \
  typing/subst typing/predef \
  typing/datarepr typing/env \
  typing/typedtree typing/ctype \
  typing/printtyp typing/includeclass \
  typing/mtype typing/includecore \
  typing/includemod typing/parmatch \
  typing/typetexp typing/stypes typing/typecore \
  typing/typedecl typing/typeclass \
  typing/typemod

COMP=bytecomp/lambda bytecomp/printlambda \
  bytecomp/typeopt bytecomp/switch bytecomp/matching \
  bytecomp/translobj bytecomp/translcore \
  bytecomp/translclass bytecomp/translmod \
  bytecomp/simplif bytecomp/runtimedef

BYTECOMP=bytecomp/meta bytecomp/instruct bytecomp/bytegen \
  bytecomp/printinstr bytecomp/opcodes bytecomp/emitcode \
  bytecomp/bytesections bytecomp/dll bytecomp/symtable \
  bytecomp/bytelink bytecomp/bytelibrarian bytecomp/bytepackager

DRIVER=driver/pparse driver/errors driver/compile \
  driver/main_args

COMPMODULES=  $(addprefix ../, $(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(DRIVER)) 

OBJS=		$(addsuffix .cmo, $(MODULES))
COMPOBJS=	$(addsuffix .cmo, $(COMPMODULES))

XOBJS=		$(addsuffix .cmx, $(MODULES))
COMPXOBJS=	$(addsuffix .cmx, $(COMPMODULES))

all: ocamlspot 

.PHONY: test

tests:
	(cd tests; $(MAKE))

ocamlspot: $(COMPOBJS) $(OBJS)
	$(OCAMLC) -o $@ $(COMPFLAGS) $(COMPOBJS) unix.cma $(OBJS)

opt.opt: ocamlspot.opt

ocamlspot.opt: $(COMPXOBJS) $(XOBJS)
	(cd ../asmrun; $(MAKE) meta.o dynlink.o)
	$(OCAMLOPT) -o $@ $(COMPFLAGS) $(COMPXOBJS) unix.cmxa $(XOBJS) \
	  ../asmrun/meta.o ../asmrun/dynlink.o -cclib "$(BYTECCLIBS)"

#	$(CAMLOPT) $(LINKFLAGS) -ccopt "$(BYTECCLINKOPTS)" -o ocamlc.opt \
#	  $(COMPOBJS:.cmo=.cmx) \
#	  asmrun/meta.o asmrun/dynlink.o -cclib "$(BYTECCLIBS)"
#	@sed -e 's|@compiler@|$$topdir/ocamlc.opt|' \
#	  driver/ocamlcomp.sh.in > ocamlcomp.sh
#	@chmod +x ocamlcomp.sh


opt: ocamlspot.opt

.PHONY: opt opt.opt

clean:
	rm -f ocamlspot ocamlspot.opt *.cm* *.o *.annot *.sp*t 
	(cd tests; $(MAKE) clean)

# generic rules :
#################

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLPP) $(COMPFLAGS) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

beforedepend::

depend: beforedepend
	$(CAMLRUN) ../tools/ocamldep $(INCLUDES) -I ../typing *.mli *.ml > .depend

.PHONY: clean install installopt beforedepend depend test

# elisp

EMACS=emacs

ELISPS=ocamlspot.el

COMPILECMD=(progn \
		(setq load-path (cons "." load-path)) \
		(byte-compile-file "ocamlspot.el"))

install-elisp:
	@if test "$(EMACSDIR)" = ""; then \
          set xxx `($(EMACS) --batch --eval "(mapcar 'print load-path)") \
                   2>/dev/null | \
                   sed -n -e '/\/site-lisp/s/"//gp'`; \
          if test "$$2" = ""; then \
            echo "Cannot determine Emacs site-lisp directory"; \
            exit 2; \
          else \
            $(MAKE) EMACSDIR="$$2" simple-install; \
	  fi; \
        else \
          $(MAKE) simple-install; \
        fi

# install the .el files, but do not compile them.
install-el:
	$(MAKE) NOCOMPILE=true install

simple-install:
	@echo "Installing in $(EMACSDIR)..."
	if test -d $(EMACSDIR); then : ; else mkdir -p $(EMACSDIR); fi
	cp $(ELISPS) $(EMACSDIR)
	if [ -z "$(NOCOMPILE)" ]; then \
	  cd $(EMACSDIR); $(EMACS) --batch --eval '$(COMPILECMD)'; \
	fi

install installopt::
	cp ocamlspot $(BINDIR)/ocamlspot$(EXE)
	if test -f ocamlspot.opt; \
	  then cp ocamlspot.opt $(BINDIR)/ocamlspot.opt$(EXE); else :; fi
	# The following is optional
	# $(MAKE) install-emacs-lisp

test: ocamlspot ocamlspot.cmo
	tests/auto-test.pl ocamlspot.ml treeset.ml xset.ml 

include .depend
