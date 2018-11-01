JAVA_PKGDIR = org/tiian/lixa/xta

liblixta_java_la_CPPFLAGS = -I$(top_srcdir)/src/common \
	-I$(top_srcdir)/src/client -I$(top_srcdir)/src/xta
liblixta_java_la_LIBADD = ../liblixta.la
liblixta_java_la_SOURCES = xta_java.c

#dist_noinst_HEADERS = org_tiian_lixa_xta_TransactionManager.h
dist_noinst_DATA = LixaErrorCodes.awk \
	$(JAVA_PKGDIR)/Xta.java $(JAVA_PKGDIR)/XtaException.java \
	$(JAVA_PKGDIR)/TransactionManager.java

javadir = $(datadir)/$(PACKAGE)/java
dist_java_DATA = xta.jar
lib_LTLIBRARIES = liblixta_java.la

SUFFIXES: .java .class

$(JAVA_PKGDIR)/ErrorCodes.java:	LixaErrorCodes.awk ../../common/lixa_errors.h
	mkdir -p $(JAVA_PKGDIR)
	$(AWK) -f $^ > $@

#$(JAVA_PKGDIR)/ErrorCodes.class:	$(JAVA_PKGDIR)/ErrorCodes.java
#if COND_HAVE_JAVAH
#	$(JAVAC) $<
#else
#	$(JAVAC) -h . $<
#endif

.java.class:
if COND_HAVE_JAVAH
	$(JAVAC) $<
else
	$(JAVAC) -h . $<
endif

#$(JAVA_PKGDIR)/TransactionManager.class:	$(JAVA_PKGDIR)/TransactionManager.java
#if COND_HAVE_JAVAH
#	$(JAVAC) $<
#else
#	$(JAVAC) -h . $<
#endif

xta.jar:	$(JAVA_PKGDIR)/ErrorCodes.class \
	$(JAVA_PKGDIR)/Xta.class $(JAVA_PKGDIR)/XtaException.class \
	$(JAVA_PKGDIR)/TransactionManager.class
	$(JAR) cf $@ $^

uninstall-hook:
	if test -d ${pkgdatadir}/java; then rmdir ${pkgdatadir}/java; fi

