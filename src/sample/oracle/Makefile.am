AM_CPPFLAGS = -I$(srcdir)/../../client @ORACLE_OCI_CFLAGS@
AM_CFLAGS =
AM_LDFLAGS = -lpthread @LIBLTDL@ @LDFLAGS_GMODULE@
# as documented in Oracle books the transaction manager library with
# ax_reg/ax_unreg functions must be linked before the Oracle client library
LDADD = ../../common/liblixabmt.la ../../client/liblixacmt.la @ORACLE_OCI_LDFLAGS@
lixa_oracle_sample_SOURCES=lixa_oracle_sample.c
bin_PROGRAMS = lixa_oracle_sample
