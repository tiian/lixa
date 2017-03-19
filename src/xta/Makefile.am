includedir = $(prefix)/include/xta
include_HEADERS = xta.h xta_transaction_manager.h
lib_LTLIBRARIES = liblixta.la
liblixta_la_SOURCES = xta_transaction_manager.c
AM_CFLAGS = -Wall -fno-strict-aliasing
liblixta_la_CPPFLAGS = -I$(top_srcdir)/src/common
liblixta_la_LIBADD =
liblixta_la_LDFLAGS = -version-info @LT_LIXTA_CURRENT@:@LT_LIXTA_REVISION@:@LT_LIXTA_AGE@
