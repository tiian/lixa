docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)/examples/cobol
doc_DATA = README EXAMPLE1.cob EXAMPLE5_PQL.cob EXAMPLE14_ORA.pco
EXTRA_DIST = $(doc_DATA)

uninstall-hook:
	if test -d ${docdir}; then rmdir ${docdir}; fi
