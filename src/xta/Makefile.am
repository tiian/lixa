includedir = $(prefix)/include/xta
include_HEADERS = xta.h xta_errors.h xta_transaction.h \
	xta_transaction_manager.h xta_xid.h \
	xta_xa_resource.h xta_native_xa_resource.h xta_acquired_xa_resource.h
lib_LTLIBRARIES = liblixta.la
liblixta_la_SOURCES = xta_errors.c xta_transaction.c \
	xta_transaction_manager.c xta_xid.c \
	xta_xa_resource.c xta_native_xa_resource.c xta_acquired_xa_resource.c
AM_CFLAGS = -Wall -fno-strict-aliasing
liblixta_la_CPPFLAGS = -I$(top_srcdir)/src/common
liblixta_la_LIBADD =
liblixta_la_LDFLAGS = -version-info @LT_LIXTA_CURRENT@:@LT_LIXTA_REVISION@:@LT_LIXTA_AGE@
