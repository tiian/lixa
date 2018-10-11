docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)/examples/xta/python
doc_DATA = README.md example_xta_sa21.py example_xta_cbpa21.py \
	example_xta_cbpa22.py
EXTRA_DIST = $(doc_DATA)

uninstall-hook:
	if test -d ${docdir}; then rmdir ${docdir}; fi
