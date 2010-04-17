AM_CPPFLAGS = -I$(srcdir)/../../client @IBMDB2_CLI_CFLAGS@
AM_CFLAGS =
AM_LDFLAGS = -lpthread @LIBLTDL@ @LDFLAGS_GMODULE@
LDADD = ../../common/liblixabmt.la ../../client/liblixacmt.la @IBMDB2_CLI_LDFLAGS@
lixa_db2_cli_sample_SOURCES=lixa_db2_cli_sample.c
bin_PROGRAMS = lixa_db2_cli_sample
