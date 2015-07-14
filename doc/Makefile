BUILDDIR = $(TOPDIR)/_build

PKGS = -package slap,gsl
DOCFLAGS = $(PKGS) -d $(TOPDIR)/doc/html -i $(BUILDDIR)/doc/custom -g odoc_custom.cma
SRCS = \
	crf.ml \
	crf_graph.mli \
	crf_model.mli \
	crf_core.mli

all:
	cd $(BUILDDIR)/lib; ocamlfind ocamldoc $(DOCFLAGS) $(SRCS)
