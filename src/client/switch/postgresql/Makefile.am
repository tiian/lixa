lib_LTLIBRARIES = liblixapq.la switch_postgresql_stareg.la
liblixapq_la_SOURCES = liblixapq.c
switch_postgresql_stareg_la_SOURCES = postgresql_stareg.c
AM_CPPFLAGS = @POSTGRESQL_CPPFLAGS@ -I../../../common
AM_CFLAGS = -Wall
AM_LDFLAGS = @POSTGRESQL_LDFLAGS@ -module
switch_postgresql_stareg_la_LIBADD = liblixapq.la
liblixapq_la_CPPFLAGS = -DLIXA_MULTITHREAD=1 -I$(srcdir)/../../../common @POSTGRESQL_CPPFLAGS@
liblixapq_la_LIBADD = ../../../common/liblixab.la
#liblixapq_la_LDFLAGS = @LDFLAGS_GLIB@ @LDFLAGS_GMODULE@ @LDFLAGS_GTHREAD@ @LDFLAGS_XML2@ -version-info @LT_LIXAC_CURRENT@:@LT_LIXAC_REVISION@:@LT_LIXAC_AGE@
liblixapq_la_LDFLAGS = @POSTGRESQL_LDFLAGS@ -version-info @LT_LIXAC_CURRENT@:@LT_LIXAC_REVISION@:@LT_LIXAC_AGE@
