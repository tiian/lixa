if COND_DOXYGEN
  MAYBE_API = lixa/html/index.html
if COMPILE_XTA
    MAYBE_XTA = xta/html/index.html
endif
endif
EXTRA_DIST = $(MAYBE_API) $(MAYBE_XTA) \
	doxygen_footer.html doxygen_header_lixa.html doxygen_header_xta.html
dist_noinst_DATA = doxygen.xta.in doxygen.lixa.in

doxygen.xta:   doxygen.xta.in
	sed \
        -e 's|@PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g' \
        -e 's|@HAVE_DOT_BOOL[@]|$(HAVE_DOT_BOOL)|g' \
        $< >$@

doxygen.lixa:   doxygen.lixa.in 
	sed \
        -e 's|@PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g' \
        -e 's|@HAVE_DOT_BOOL[@]|$(HAVE_DOT_BOOL)|g' \
        $< >$@

all:	$(MAYBE_API) $(MAYBE_XTA)

if COND_DOXYGEN
xta/html/index.html:	doxygen.xta $(top_srcdir)/src/xta/*.h \
	doxygen_footer.html doxygen_header_xta.html
	doxygen doxygen.xta
lixa/html/index.html:	doxygen.lixa $(top_srcdir)/src/client/*.h \
	$(top_srcdir)/src/common/*.h $(top_srcdir)/src/server/*.h \
	$(top_srcdir)/src/xta/*.h doxygen_footer.html doxygen_header_lixa.html
	doxygen doxygen.lixa
else
xta/html/index.html:	doxygen.xta $(top_srcdir)/src/xta/*.h \
	doxygen_footer.html doxygen_header_xta.html
	@echo "XTA API documentation skipped because doxygen is not available"
lixa/html/index.html:	doxygen.lixa $(top_srcdir)/src/client/*.h \
	$(top_srcdir)/src/common/*.h $(top_srcdir)/src/server/*.h \
	$(top_srcdir)/src/xta/*.h doxygen_footer.html doxygen_header_lixa.html
	@echo "LIXA source code documentation skipped because doxygen is not available"
endif

