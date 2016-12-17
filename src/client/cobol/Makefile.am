dist_noinst_DATA = README
include_HEADERS = TXSTATUS.cpy TXINFDEF.cpy LIXAXID.cpy
noinst_HEADERS = lixa_tx_cobol.h
lib_LTLIBRARIES = liblixacobol.la
if COMPILE_POSTGRESQL_STUFF
  LIXAPQ_H_INCLUDE = -I../switch/postgresql @POSTGRESQL_CPPFLAGS@
endif
liblixacobol_la_SOURCES = lixa_tx_cobol.c
AM_CFLAGS = -Wall -fno-strict-aliasing
liblixacobol_la_CPPFLAGS = -DLIXA_MULTITHREAD=1 -I.. -I../../common $(LIXAPQ_H_INCLUDE)
liblixacobol_la_LIBADD = ../liblixac.la
liblixacobol_la_LDFLAGS = @LDFLAGS_GLIB@ @LDFLAGS_GMODULE@ @LDFLAGS_GTHREAD@ @LDFLAGS_XML2@ -version-info @LT_LIXAC_CURRENT@:@LT_LIXAC_REVISION@:@LT_LIXAC_AGE@
