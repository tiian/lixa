JAVA_PKGDIR = org/tiian/lixa/xta

liblixta_java_la_LIBADD = ../liblixta.la
liblixta_java_la_SOURCES = xta_java.c

#dist_noinst_HEADERS = org_tiian_lixa_xta_TransactionManager.h
dist_noinst_DATA = LixaErrorCodes.awk

javadir = $(datadir)/$(PACKAGE)/java
dist_java_DATA = xta.jar
lib_LTLIBRARIES = liblixta_java.la

$(JAVA_PKGDIR)/ErrorCodes.java:	LixaErrorCodes.awk ../../common/lixa_errors.h
	mkdir -p $(JAVA_PKGDIR)
	$(AWK) -f $^ > $@

$(JAVA_PKGDIR)/ErrorCodes.class:	$(JAVA_PKGDIR)/ErrorCodes.java
if COND_HAVE_JAVAH
	$(JAVAC) $<
else
	$(JAVAC) -h . $<
endif

xta.jar:	$(JAVA_PKGDIR)/ErrorCodes.class
	$(JAR) cf $@ $^

uninstall-hook:
	if test -d ${pkgdatadir}/java; then rmdir ${pkgdatadir}/java; fi

