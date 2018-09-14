if COMPILE_XTA_CPP
  MAYBE_XTA_CPP = cpp
endif
if COMPILE_MYSQL_STUFF
  MAYBE_MYSQL_SOURCES = xta_mysql_xa_resource.c
  MAYBE_MYSQL_CPPFLAGS = @MYSQL_CPPFLAGS@ \
	-I$(top_srcdir)/src/client/switch/mysql
  MAYBE_MYSQL_LIBADD = $(top_srcdir)/src/client/switch/mysql/liblixamy.la
endif
if COMPILE_POSTGRESQL_STUFF
  MAYBE_POSTGRESQL_SOURCES = xta_postgresql_xa_resource.c
  MAYBE_POSTGRESQL_CPPFLAGS = @POSTGRESQL_CPPFLAGS@ \
	-I$(top_srcdir)/src/client/switch/postgresql
  MAYBE_POSTGRESQL_LIBADD = $(top_srcdir)/src/client/switch/postgresql/liblixapq.la
endif

SUBDIRS = . $(MAYBE_XTA_CPP)

includedir = $(prefix)/include/xta
include_HEADERS = xta.h xta_iface.h xta_init.h xta_transaction.h \
	xta_transaction_manager.h xta_xid.h \
	xta_xa_resource.h xta_native_xa_resource.h xta_acquired_xa_resource.h \
	xta_postgresql_xa_resource.h xta_mysql_xa_resource.h
noinst_HEADERS = xta.h.in
lib_LTLIBRARIES = liblixta.la
liblixta_la_SOURCES = xta_init.c xta_transaction.c \
	xta_transaction_manager.c xta_xid.c \
	xta_xa_resource.c xta_native_xa_resource.c xta_acquired_xa_resource.c \
	$(MAYBE_MYSQL_SOURCES) $(MAYBE_POSTGRESQL_SOURCES)
AM_CFLAGS = -Wall -fno-strict-aliasing
liblixta_la_CPPFLAGS = -I$(top_srcdir)/src/common -I$(top_srcdir)/src/client \
	$(MAYBE_MYSQL_CPPFLAGS) $(MAYBE_POSTGRESQL_CPPFLAGS)
liblixta_la_LIBADD = $(top_srcdir)/src/common/liblixab.la \
	$(top_srcdir)/src/client/liblixac.la \
	$(MAYBE_MYSQL_LIBADD) $(MAYBE_POSTGRESQL_LIBADD) \
	@LDFLAGS_GLIB@ @LDFLAGS_XML2@
liblixta_la_LDFLAGS = -version-info @LT_LIXTA_CURRENT@:@LT_LIXTA_REVISION@:@LT_LIXTA_AGE@
