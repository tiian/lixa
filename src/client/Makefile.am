SUBDIRS = switch
include_HEADERS = tx.h xa.h
noinst_HEADERS = client_config.h client_conn.h client_load.h client_status.h
lib_LTLIBRARIES = liblixac.la liblixacmt.la
liblixac_la_SOURCES = client_config.c client_conn.c client_load.c \
	client_status.c tx.c
liblixacmt_la_SOURCES = client_config.c client_conn.c client_load.c \
	client_status.c tx.c
AM_CPPFLAGS = -I$(srcdir)/../common 
AM_CFLAGS = -Wall -fno-strict-aliasing
liblixac_la_LIBADD = ../common/liblixab.la
liblixacmt_la_LIBADD = ../common/liblixabmt.la
liblixac_la_LDFLAGS = @LDFLAGS_XML2@ @LDFLAGS_GLIB@ @LDFLAGS_GMODULE@ -version-info @LT_LIXAC_CURRENT@:@LT_LIXAC_REVISION@:@LT_LIXAC_AGE@
liblixacmt_la_LDFLAGS = -lpthread @LDFLAGS_XML2@ @LDFLAGS_GLIB@ @LDFLAGS_GMODULE@ -version-info @LT_LIXAC_CURRENT@:@LT_LIXAC_REVISION@:@LT_LIXAC_AGE@
