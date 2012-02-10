include_HEADERS = lixamy.h
noinst_HEADERS = liblixamy.h
lib_LTLIBRARIES = liblixamy.la switch_mysql_stareg.la
liblixamy_la_SOURCES = liblixamy.c
switch_mysql_stareg_la_SOURCES = mysql_stareg.c
AM_CPPFLAGS = @MYSQL_CPPFLAGS@ -I../../../common
AM_CFLAGS = -Wall
AM_LDFLAGS = @MYSQL_LDFLAGS@ @LDFLAGS_GLIB@ -module
AM_LIBS = $(srcdir)/../../../common/liblixab.la
switch_mysql_stareg_la_LIBADD = liblixamy.la
liblixamy_la_CPPFLAGS = -DLIXA_MULTITHREAD=1 -I$(srcdir)/../../../common @MYSQL_CPPFLAGS@
liblixamy_la_LIBADD = ../../../common/liblixab.la
liblixamy_la_LDFLAGS = @MYSQL_LDFLAGS@ -version-info @LT_LIXAC_CURRENT@:@LT_LIXAC_REVISION@:@LT_LIXAC_AGE@
