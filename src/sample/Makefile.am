AM_CPPFLAGS = -I$(srcdir)/../client
AM_CFLAGS = -Wall
AM_LDFLAGS = -lpthread
# remove this legacy stuff (2009-08-16)
#cdemo81_CPPFLAGS = -I/usr/lib/oracle/xe/app/oracle/product/10.2.0/server/rdbms/public/
#cdemo81_LDFLAGS = -L/usr/lib/oracle/xe/app/oracle/product/10.2.0/server/lib -lclntsh -lnnz10 -lpthread 
LDADD = ../client/liblixacmt.la
hello_SOURCES = hello.c
cdemo81_CPPFLAGS = @ORACLE_OCI_CFLAGS@
cdemo81_CFLAGS =
cdemo81_LDFLAGS = @ORACLE_OCI_LDFLAGS@
cdemo81_SOURCES = cdemo81.c
bin_PROGRAMS = cdemo81
noinst_PROGRAMS = hello

