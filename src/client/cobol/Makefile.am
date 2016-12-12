dist_noinst_DATA = README
include_HEADERS = TXSTATUS.cpy TXINFDEF.cpy
lib_LTLIBRARIES = liblixacobol.la
liblixacobol_la_SOURCES = lixa_tx_cobol.c
AM_CFLAGS = -Wall -fno-strict-aliasing
liblixacobol_la_CPPFLAGS = -DLIXA_MULTITHREAD=1 -I..
liblixacobol_la_LIBADD = ../liblixac.la
liblixacobol_la_LDFLAGS = @LDFLAGS_GLIB@ @LDFLAGS_GMODULE@ @LDFLAGS_GTHREAD@ @LDFLAGS_XML2@ -version-info @LT_LIXAC_CURRENT@:@LT_LIXAC_REVISION@:@LT_LIXAC_AGE@
