if COND_DOXYGEN
  MAYBE_API = lixa/html/index.html
if COMPILE_XTA
    MAYBE_XTA = xta/C/html/index.html
if COMPILE_XTA_CPP
      MAYBE_XTA_CPP = xta/CPP/html/index.html
endif
if COMPILE_XTA_JAVA
      MAYBE_XTAJAVA = xta/Java/html/index.html
      JAVA_PKGDIR = org/tiian/lixa/xta
endif
endif
endif
EXTRA_DIST = $(MAYBE_API) $(MAYBE_XTA) $(MAYBE_XTA_CPP) $(MAYBE_XTAJAVA) \
	doxygen_footer.html doxygen_header_lixa.html \
	doxygen_header_xta.html doxygen_header_xta-cpp.html
dist_noinst_DATA = doxygen.xta.in doxygen.xta-cpp.in doxygen.lixa.in

doxygen.xta:   doxygen.xta.in
	sed \
        -e 's|@PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g' \
        -e 's|@HAVE_DOT_BOOL[@]|$(HAVE_DOT_BOOL)|g' \
        $< >$@

doxygen.xta-cpp:   doxygen.xta-cpp.in
	sed \
        -e 's|@PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g' \
        -e 's|@HAVE_DOT_BOOL[@]|$(HAVE_DOT_BOOL)|g' \
        $< >$@

doxygen.lixa:   doxygen.lixa.in 
	sed \
        -e 's|@PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g' \
        -e 's|@HAVE_DOT_BOOL[@]|$(HAVE_DOT_BOOL)|g' \
        $< >$@

all:	$(MAYBE_API) $(MAYBE_XTA) $(MAYBE_XTA_CPP) $(MAYBE_XTAJAVA)

if COND_DOXYGEN
xta/C/html/index.html:	doxygen.xta \
	$(top_srcdir)/src/xta/*.h \
	$(top_srcdir)/src/xta/*.c \
	doxygen_footer.html doxygen_header_xta.html
	$(MKDIR_P) xta/C
	doxygen doxygen.xta
xta/CPP/html/index.html:	doxygen.xta-cpp \
	$(top_srcdir)/src/xta/cpp/*.hpp \
	$(top_srcdir)/src/xta/cpp/*.cpp \
	doxygen_footer.html doxygen_header_xta-cpp.html
	$(MKDIR_P) xta/CPP
	doxygen doxygen.xta-cpp
lixa/html/index.html:	doxygen.lixa $(top_srcdir)/src/client/*.h \
	$(top_srcdir)/src/common/*.h $(top_srcdir)/src/server/*.h \
	$(top_srcdir)/src/xta/*.h doxygen_footer.html doxygen_header_lixa.html
	doxygen doxygen.lixa
else
xta/C/html/index.html:	doxygen.xta $(top_srcdir)/src/xta/*.h \
	doxygen_footer.html doxygen_header_xta.html
	@echo "XTA C API documentation skipped because doxygen is not available"
xta/CPP/html/index.html:	doxygen.xta-cpp $(top_srcdir)/src/xta/*.h \
	doxygen_footer.html doxygen_header_xta-cpp.html
	@echo "XTA C++ API documentation skipped because doxygen is not available"
lixa/html/index.html:	doxygen.lixa $(top_srcdir)/src/client/*.h \
	$(top_srcdir)/src/common/*.h $(top_srcdir)/src/server/*.h \
	$(top_srcdir)/src/xta/*.h doxygen_footer.html doxygen_header_lixa.html
	@echo "LIXA source code documentation skipped because doxygen is not available"
endif

if COND_JAVADOC
$(MAYBE_XTAJAVA):	$(top_srcdir)/src/xta/java/$(JAVA_PKGDIR)/*.java
	$(JAVADOC) org.tiian.lixa.xta -d $(top_srcdir)/doc/api/xta/Java/html \
	-sourcepath $(top_srcdir)/src/xta/java
else
$(MAYBE_XTAJAVA):	$(top_srcdir)/src/xta/java/$(JAVA_PKGDIR)/*.java
	@echo "XTA Java API documentation skipped because javadoc is not available"
endif
