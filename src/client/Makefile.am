if COND_COBOLAPI
  COBOL_COPYBOOKS = TXSTATUS.cpy TXINFDEF.cpy LIXAXID.cpy
  COBOL_INCLUDE = lixa_tx_cobol.h
  COBOL_SOURCE = lixa_tx_cobol.c
endif
SUBDIRS = . switch
include_HEADERS = lixa.h lixanonapi.h tx.h $(COBOL_COPYBOOKS)
noinst_HEADERS = client_config.h client_conn.h client_recovery.h \
	client_status.h lixa_tx.h lixa_tx_rc.h lixa_xa.h client_tpm.h \
	$(COBOL_INCLUDE)
lib_LTLIBRARIES = liblixac.la
liblixac_la_SOURCES = client_config.c client_conn.c client_recovery.c \
	client_status.c lixa_ax.c lixa_tx.c lixa_tx_rc.c lixa_xa.c tx.c \
	lixanonapi.c client_tpm.c $(COBOL_SOURCE)
AM_CFLAGS = -Wall -fno-strict-aliasing
liblixac_la_CPPFLAGS = -DLIXA_MULTITHREAD=1 -I$(srcdir)/../common
liblixac_la_LIBADD = ../common/liblixab.la
liblixac_la_LDFLAGS = @LDFLAGS_GLIB@ @LDFLAGS_GMODULE@ @LDFLAGS_GTHREAD@ @LDFLAGS_XML2@ -version-info @LT_LIXAC_CURRENT@:@LT_LIXAC_REVISION@:@LT_LIXAC_AGE@
bin_PROGRAMS = lixar lixat lixau lixavsr lixa-config lixatpm
lixar_CPPFLAGS = -I$(srcdir)/../common
lixar_LDADD = ../common/liblixab.la liblixac.la
lixar_LDFLAGS = @LDFLAGS_GLIB@ @LDFLAGS_XML2@
lixar_SOURCES = lixar.c
lixat_CPPFLAGS = -I$(srcdir)/../common
lixat_LDADD = ../common/liblixab.la liblixac.la
lixat_LDFLAGS = @LDFLAGS_GTHREAD@
lixat_SOURCES = lixat.c
lixau_CPPFLAGS = -I$(srcdir)/../common
lixau_LDADD = ../common/liblixab.la @LDFLAGS_XML2@ liblixac.la
lixau_LDFLAGS = @LDFLAGS_GLIB@
lixau_SOURCES = lixau.c
lixavsr_CPPFLAGS = -I$(srcdir)/../common @ORACLE_OCI_CFLAGS@ @MYSQL_CPPFLAGS@ \
	-Iswitch/mysql
lixavsr_LDADD = ../common/liblixab.la @LDFLAGS_XML2@ liblixac.la \
	@ORACLE_OCI_LIBS@ @MYSQL_LDADD@
lixavsr_LDFLAGS = @LDFLAGS_GLIB@ @ORACLE_OCI_LDFLAGS@ @MYSQL_LDFLAGS@
lixavsr_SOURCES = lixavsr.c
lixa_config_SOURCES = lixa-config.c
lixa_config_LDFLAGS = @LDFLAGS_GLIB@
lixatpm_CPPFLAGS = -I$(srcdir)/../common
lixatpm_LDADD = ../common/liblixab.la liblixac.la
lixatpm_LDFLAGS = @LDFLAGS_GLIB@ @LDFLAGS_XML2@
lixatpm_SOURCES = lixatpm.c
