docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)/examples/xta/java
doc_DATA = README.md ExampleXtaSA31.java ExampleXtaMACC31.java \
	ExampleXtaMACBPA31.java ExampleXtaMACBPA32.java \
	ExampleXtaMACBPS31.java ExampleXtaMACBPS32.java
dist_noinst_DATA = README.md.in ExampleXtaSA31.java.in
EXTRA_DIST = $(doc_DATA)

README.md:	README.md.in
	sed -e 's|@MYSQL_JDBC_JAR[@]|$(MYSQL_JDBC_JAR)|g' $< >$@
ExampleXtaSA31.java:	ExampleXtaSA31.java.in
	sed -e 's|@MYSQL_JDBC_DATASOURCE_CLASS[@]|$(MYSQL_JDBC_DATASOURCE_CLASS)|g' $< >$@

uninstall-hook:
	if test -d ${docdir}; then rmdir ${docdir}; fi
