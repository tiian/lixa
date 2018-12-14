JAVA_PKGDIR = org/tiian/lixa/xta

liblixta_java_la_CPPFLAGS = -I$(top_srcdir)/src/common \
	-I$(top_srcdir)/src/client -I$(top_srcdir)/src/xta
liblixta_java_la_LIBADD = ../liblixta.la
liblixta_java_la_SOURCES = error_codes_jni.c transaction_manager_jni.c \
	transaction_jni.c xta_jni.c xta_java_xa_resource.c xtaxid_jni.c \
	xtaexception_jni.c

dist_noinst_DATA = LixaErrorCodes.awk \
	$(JAVA_PKGDIR)/Xta.java $(JAVA_PKGDIR)/XtaException.java \
	$(JAVA_PKGDIR)/XtaXid.java $(JAVA_PKGDIR)/Transaction.java \
	$(JAVA_PKGDIR)/TransactionManager.java \
	$(JAVA_PKGDIR)/LixaDummyXAResource.java \
	$(JAVA_PKGDIR)/LixaMonkeyXAResource.java
dist_noinst_HEADERS = transaction_jni.h xta_java_xa_resource.h \
	xtaexception_jni.h xtaxid_jni.h

javadir = $(datadir)/$(PACKAGE)/java
dist_java_DATA = xta.jar
lib_LTLIBRARIES = liblixta_java.la

SUFFIXES: .java .class

$(JAVA_PKGDIR)/ErrorCodes.java:	LixaErrorCodes.awk ../../common/lixa_errors.h
	mkdir -p $(JAVA_PKGDIR)
	$(AWK) -f $^ > $@

.java.class:
	$(JAVAC) $<

xta.jar:	$(JAVA_PKGDIR)/ErrorCodes.class \
	$(JAVA_PKGDIR)/Xta.class $(JAVA_PKGDIR)/XtaException.class \
	$(JAVA_PKGDIR)/XtaXid.class $(JAVA_PKGDIR)/Transaction.class \
	$(JAVA_PKGDIR)/TransactionManager.class
	$(JAR) cf $@ $^

uninstall-hook:
	if test -d ${pkgdatadir}/java; then rmdir ${pkgdatadir}/java; fi

