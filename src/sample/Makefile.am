AM_CPPFLAGS = -I$(srcdir)/../client
AM_CFLAGS = -Wall
AM_LDFLAGS = -lpthread @LDFLAGS_GMODULE@
LDADD = ../common/liblixab.la ../client/liblixac.la
noinst_PROGRAMS = hello test
hello_SOURCES = hello.c
test_SOURCES = test.c
