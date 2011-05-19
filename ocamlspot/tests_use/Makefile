include ../../config/Makefile

# Various commands and dir
##########################
CAMLRUN=../../boot/ocamlrun
OCAMLC   = ../../ocamlcomp.sh -annot
OCAMLOPT = ../../ocamlcompopt.sh
OCAMLDEP = $(CAMLRUN) ../../tools/ocamldep
OCAMLLEX = $(CAMLRUN) ../../boot/ocamllex
OCAMLYACC= ../../boot/ocamlyacc
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
	-I $(OCAMLSRCDIR)/toplevel/ \
	-I dir1 -I dir2

OTHERS=../../otherlibs

# Requires unix!
COMPFLAGS= $(INCLUDES_DEP) -I $(OTHERS)/unix

include Makefile.targets

all: $(TARGETS) 

clean:
	rm -f *.cm* *.o *.annot *.sp*t  */*.cm* */*.annot */*.sp*t

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
	$(CAMLRUN) ../../tools/ocamldep $(INCLUDES) -I dir1 -I dir2 *.mli *.ml */*.mli */*.ml > .depend

Makefile.targets: *.ml *.mli
	echo TARGETS= \\ > $@
	ls *.ml *.mli | sed -e 's/mli$$/cmi/' -e 's/ml$$/cmo/' -e 's/$$/ \\/'	 >> $@

.PHONY: clean install installopt beforedepend depend

include .depend
