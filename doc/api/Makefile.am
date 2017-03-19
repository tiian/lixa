if COND_DOXYGEN
  MAYBE_API = xta/html/index.html lixa/html/index.html
endif
EXTRA_DIST = $(MAYBE_API)
dist_noinst_DATA = doxygen.xta.in doxygen.lixa.in

doxygen.xta:   doxygen.xta.in
	sed \
        -e 's|@PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g' \
        -e 's|@HAVE_DOT[@]|$(HAVE_DOT)|g' \
        $< >$@

doxygen.lixa:   doxygen.lixa.in
	sed \
        -e 's|@PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g' \
        -e 's|@HAVE_DOT[@]|$(HAVE_DOT)|g' \
        $< >$@

all:	$(MAYBE_API)

xta/html/index.html:	doxygen.xta $(top_srcdir)/src/xta/*.h
	doxygen doxygen.xta

lixa/html/index.html:	doxygen.lixa $(top_srcdir)/src/client/*.h \
	$(top_srcdir)/src/common/*.h $(top_srcdir)/src/server/*.h
	doxygen doxygen.lixa
