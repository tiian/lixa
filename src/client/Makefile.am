SUBDIRS = switch
include_HEADERS = tx.h
noinst_HEADERS = client_config.h client_conn.h client_recovery.h \
	client_status.h lixa_tx.h lixa_tx_rc.h lixa_xa.h
lib_LTLIBRARIES = liblixac.la liblixacmt.la
liblixac_la_SOURCES = client_config.c client_conn.c client_recovery.c \
	client_status.c lixa_ax.c lixa_tx.c lixa_tx_rc.c lixa_xa.c tx.c
liblixacmt_la_SOURCES = client_config.c client_conn.c client_recovery.c \
	client_status.c lixa_ax.c lixa_tx.c lixa_tx_rc.c lixa_xa.c tx.c
AM_CPPFLAGS = -I$(srcdir)/../common 
AM_CFLAGS = -Wall -fno-strict-aliasing
liblixac_la_LIBADD = ../common/liblixab.la
liblixacmt_la_LIBADD = ../common/liblixabmt.la
liblixac_la_LDFLAGS = @LDFLAGS_GLIB@ @LDFLAGS_GMODULE@ @LDFLAGS_XML2@ -version-info @LT_LIXAC_CURRENT@:@LT_LIXAC_REVISION@:@LT_LIXAC_AGE@
liblixacmt_la_LDFLAGS = @LDFLAGS_GLIB@ @LDFLAGS_GMODULE@ @LDFLAGS_GTHREAD@ @LDFLAGS_XML2@ -version-info @LT_LIXAC_CURRENT@:@LT_LIXAC_REVISION@:@LT_LIXAC_AGE@
bin_PROGRAMS = lixar
lixar_LDADD = ../common/liblixabmt.la liblixacmt.la
lixar_SOURCES = lixar.c
