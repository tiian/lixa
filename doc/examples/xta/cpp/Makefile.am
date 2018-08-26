docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)/examples/xta/cpp
doc_DATA = README.md example_xta_sa11.cpp example_xta_macc11.cpp \
	example_xta_macbps11.cpp example_xta_macbps12.cpp 
EXTRA_DIST = $(doc_DATA)

uninstall-hook:
	if test -d ${docdir}; then rmdir ${docdir}; fi
