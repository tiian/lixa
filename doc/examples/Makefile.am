SUBDIRS = cobol
docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)/examples
doc_DATA = example1.c example2_ora.c example3_db2.c example4_ora_db2.c \
	example5_pql.c example6_pql_ora.c example7_pql_db2.c example8_mys.c \
	example9_mys_pql.c example10_mys_pql_ora.c example11_mys_mys.c \
	example12_wmq.c example13_wmq_mys_pql.c example14_ora.pc \
	example15_transaction_coupling.c
EXTRA_DIST = $(doc_DATA)

uninstall-hook:
	if test -d ${docdir}; then rmdir ${docdir}; fi
