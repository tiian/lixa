docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)/examples/xta/java
doc_DATA = README.md ExampleXtaSA31.java ExampleXtaMACC31.java \
	ExampleXtaMACBPA31.java ExampleXtaMACBPA32.java \
	ExampleXtaMACBPS31.java ExampleXtaMACBPS32.java
EXTRA_DIST = $(doc_DATA)

uninstall-hook:
	if test -d ${docdir}; then rmdir ${docdir}; fi
