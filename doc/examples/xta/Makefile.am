docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)/examples/xta
doc_DATA = README.md example_xta_sa01.c example_xta_macc01.c \
	example_xta_macbps01.c example_xta_macbps02.c \
	example_xta_macbpa01.c example_xta_macbpa02.c
EXTRA_DIST = $(doc_DATA)

uninstall-hook:
	if test -d ${docdir}; then rmdir ${docdir}; fi
