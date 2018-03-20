docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)/examples/xta
doc_DATA = README.md example_xta_sa01.c
EXTRA_DIST = $(doc_DATA)

uninstall-hook:
	if test -d ${docdir}; then rmdir ${docdir}; fi
