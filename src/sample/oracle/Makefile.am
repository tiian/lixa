AM_CPPFLAGS = -I$(srcdir)/../../client @ORACLE_OCI_CFLAGS@
AM_CFLAGS =
AM_LDFLAGS = -lpthread @LIBLTDL@ @LDFLAGS_GMODULE@
# as documented in Oracle books the transaction manager library with
# ax_reg/ax_unreg functions must be linked before the Oracle client library
LDADD = ../../common/liblixabmt.la ../../client/liblixacmt.la @ORACLE_OCI_LDFLAGS@
cdemo81_SOURCES = cdemo81.c
cdemo81_lixa_SOURCES = cdemo81_lixa.c
bin_PROGRAMS = cdemo81 cdemo81_lixa 
